import os
try:
    import cpickle as pickle
except:
    import pickle

def pickled(f):
    def new_f(*args, **kwargs):
        compressed = ''
        if len(args) > 0:
            compressed = '_' + '_'.join([str(arg)[:10] for arg in args])
        if len(kwargs) > 0:
            compressed += '_' + '_'.join([(str(k)+str(v))[:10] for k,v in kwargs])

        filename = '%s%s.pickle' % (f.__name__, compressed)

        if os.path.exists(filename):
            pickled = open(filename, 'rb')
            result = pickle.load(pickled)
            pickled.close()
        else:
            result = f(*args, **kwargs)
            pickled = open(filename, 'wb')
            pickle.dump(result, pickled)
            pickled.close()

        return result

    new_f.__name__ = f.__name__
    new_f.__doc__ = f.__doc__

    return new_f
