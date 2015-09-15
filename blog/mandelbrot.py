#!/usr/bin/env python3

import cmath
import multiprocessing
import os
import PIL.Image
import random
import time

def generate_image(width, height, generator, threads = 1):
    '''
    Generate an RGB image using a generator function.

    width, height -- the size of the generated image
    generator -- a function that takes (x, y) and returns (r, g, b)
    threads -- if != 1, use multiprocessing to spawn this many processes
    '''

    # Generate the data as a row-major list of (r, g, b)
    if threads == 1:
        data = [generator(x, y) for y in range(height) for x in range(width)]
    else:
        pool = multiprocessing.Pool(threads)
        data = pool.starmap(generator, [(x, y) for y in range(height) for x in range(width)])

    # Pack that into a Pillow image and return it
    img = PIL.Image.new('RGB', (width, height))
    img.putdata(data)
    return img

# --- coloring functions ---

def grayscale(v):
    '''Simple grayscale value.'''

    g = int(256 * v)
    return (g, g, g)

def hot_and_cold(v):
    '''Scale from black to blue to red and back to black.'''

    r = g = b = 0

    if v < 1/3:
        v = 3 * v
        b = int(256 * v)
    elif v < 2/3:
        v = 3 * (v - 1/3)
        r = int(256 * v)
        b = int(256 * (1 - v))
    else:
        v = 3 * (v - 2/3)
        r = int(256 * (1 - v))

    return (r, g, b)

def noise(v):
    '''Noise if not in the edge cases.'''

    if 0.1 < v < 0.9:
        return (
            random.randrange(0, 255),
            random.randrange(0, 255),
            random.randrange(0, 255),
        )
    else:
        return (0, 0, 0)

# --- the mandelbrot and mandelbutter ---

def make_mandelbrot_generator(width, height, center, size, max_iterations = 256, coloring = grayscale):
    '''
    A generator that makes generate_image compatible mandelbrot generators.

    width, height -- the size of the resulting image (used for scale)
    center -- the focus point of the image
    size -- the size of the larger dimension
    max_iterations -- the scale to check before exploding, used to scale the output coloring
    coloring -- takes a value [0, 1.0] (what percent of max_iterations before diverging and returns (r, g, b)
    '''

    # Scale the size so that is the size of the larger dimension
    if width >= height:
        size_x = size
        size_y = size * height / width
    else:
        size_x = size * width / height
        size_y = size

    # Convert to a bounding box
    min_x = center[0] - size_x / 2
    max_x = center[0] + size_x / 2
    min_y = center[1] - size_y / 2
    max_y = center[1] + size_y / 2

    def generator(x, y):
        # Scale to the mandlebrot frame; convert to a complex number
        x = (x / width) * (max_x - min_x) + min_x
        y = (y / height) * (max_y - min_y) + min_y
        c = x + y * 1j

        # Iterate until we escape to infinity or run out of iterations
        # For our purposes, we can consider infinity = 2
        z = 0
        for iteration in range(max_iterations):
            z = z * z + c

            # Size is r of polar coordinates
            (r, phi) = cmath.polar(z)
            if r > 2:
                break

        return coloring(iteration / max_iterations)

    return generator

# --- sample test cases ---

THREAD_COUNT = max(1, multiprocessing.cpu_count() - 1)

SIZES = [
    (400, 300),
    (1920, 1080)
]

COLORINGS = [
    ('grayscale', grayscale),
    ('hot-and-cold', hot_and_cold),
]

IMAGES = [
    ('default', (-0.5, 0), 3),
    # http://www.nahee.com/Derbyshire/manguide.html
    ('seahorse-valley', (-0.75, 0.1), 0.05),
    ('triple-spiral-valley', (0.088, 0.654), 0.25),
    ('quad-spiral-valley', (0.274, 0.482), 0.005),
    ('double-scepter-valley', (-0.1, 0.8383), 0.005),
    ('mini-mandelbrot', (-1.75, 0), 0.1),

]

for width, height in SIZES:
    for image_name, center, size in IMAGES:
        for coloring_name, coloring in COLORINGS:
            filename = os.path.join('{width}x{height}', 'mandelbrot_{name}_{width}x{height}_{coloring}.png')
            filename = filename.format(
                name = image_name,
                width = width,
                height = height,
                coloring = coloring_name,
            )
            generator = make_mandelbrot_generator(width, height, center, size, coloring = coloring)

            start = time.time()
            img = generate_image(
                width,
                height,
                generator,
                threads = THREAD_COUNT
            )
            end = time.time()

            if not os.path.exists(os.path.dirname(filename)):
                os.makedirs(os.path.dirname(filename))
            img.save(filename)

            print('{} generated in {} seconds with {} threads'.format(
                filename,
                end - start,
                THREAD_COUNT
            ))
