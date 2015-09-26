import pyparsing as pp

# https://github.com/schyntax/schyntax
# https://github.com/schyntax/schyntax/blob/master/grammar/schyntax.jison

# Literals

POSITIVE_INTEGER = pp.Regex(r'[0-9]+')
NEGATIVE_INTEGER = pp.Regex(r'-[0-9]+')

WILDCARD = pp.Regex(r'\*')

MIXIN_IDENTIFIER = pp.Regex(r'\$[a-z][a-z0-9_]*')

SUNDAY = pp.Regex(r'\b(su|sun|sunday)\b')
MONDAY = pp.Regex(r'\b(mo|mon|monday)\b')
TUESDAY = pp.Regex(r'\b(tu|tue|tuesday|tues)\b')
WEDNESDAY = pp.Regex(r'\b(we|wed|wednesday)\b')
THURSDAY = pp.Regex(r'\b(th|thu|thursday|thur|thurs)\b')
FRIDAY = pp.Regex(r'\b(fr|fri|friday)\b')
SATURDAY = pp.Regex(r'\b(sa|sat|saturday)\b')

SECONDS = pp.Regex(r'\b(s|sec|second|seconds|secondofminute|secondsofminute)\b')
MINUTES = pp.Regex(r'\b(m|min|minute|minutes|minuteofhour|minutesofhour)\b')
HOURS = pp.Regex(r'\b(h|hour|hours|hourofday|hoursofday)\b')
DAYS_OF_WEEK = pp.Regex(r'\b(day|days|dow|dayofweek|daysofweek)\b')
DAYS_OF_MONTH = pp.Regex(r'\b(dom|dayofmonth|daysofmonth)\b')
DATES = pp.Regex(r'\b(date|dates)\b')

GROUP = pp.Regex(r'\b(group)\b')

# --- Literals ---

ModulusLiteral = pp.Literal('%').suppress() + POSITIVE_INTEGER

IntegerLiteral = POSITIVE_INTEGER | NEGATIVE_INTEGER

DayLiteral = SUNDAY | MONDAY | TUESDAY | WEDNESDAY | THURSDAY | FRIDAY | SATURDAY

DayOrIntegerLiteral = DayLiteral | IntegerLiteral

DateLiteral = (
    POSITIVE_INTEGER + pp.Literal('/').suppress() + POSITIVE_INTEGER
    | POSITIVE_INTEGER + pp.Literal('/').suppress() + POSITIVE_INTEGER + pp.Literal('/').suppress() + POSITIVE_INTEGER
)

# --- Ranges ---

DayRange = (
    DayOrIntegerLiteral
    | DayOrIntegerLiteral + pp.Literal('..').suppress() + DayOrIntegerLiteral
)

IntegerRange = (
    WILDCARD
    | IntegerLiteral
    | IntegerLiteral + pp.Literal('..').suppress() + IntegerLiteral
)

DateRange = (
    DateLiteral
    | DateLiteral + pp.Literal('..').suppress() + DateLiteral
)

# --- Arguments ---

OptionalModulus = pp.Optional(ModulusLiteral)

OptionalExclude = pp.Optional(pp.Literal('!'))

DayArgument = (
    OptionalExclude + ModulusLiteral
    | OptionalExclude + DayRange + OptionalModulus
)

IntegerArgument = (
    OptionalExclude + ModulusLiteral
    | OptionalExclude + IntegerRange + OptionalModulus
)

DateArgument = (
    OptionalExclude + ModulusLiteral
    | OptionalExclude + DateRange + OptionalModulus
)

DayArgumentList = pp.delimitedList(DayArgument)

IntegerArgumentList = pp.delimitedList(IntegerArgument)

DateArgumentList = pp.delimitedList(DateArgument)

# --- Expressions ---

IntegerExpressionName = SECONDS | MINUTES | HOURS | DAYS_OF_MONTH

DateTypeExpression = (
    DATES + pp.Literal('(').suppress() + pp.Literal(')').suppress()
    | DATES + pp.Literal('(').suppress() + DateArgumentList + pp.Literal(')').suppress()
)

DayTypeExpression = (
    DAYS_OF_WEEK + pp.Literal('(').suppress() + pp.Literal(')').suppress()
    | DAYS_OF_WEEK + pp.Literal('(').suppress() + DayArgumentList + pp.Literal(')').suppress()
)

IntegerTypeExpression = (
    IntegerExpressionName + pp.Literal('(').suppress() + pp.Literal(')').suppress()
    | IntegerExpressionName + pp.Literal('(').suppress() + IntegerArgumentList + pp.Literal(')').suppress()
)

Expression = (
    IntegerTypeExpression
    | DayTypeExpression
    | DateTypeExpression
)

ExpressionList = pp.OneOrMore(Expression) | pp.delimitedList(Expression)

Group = GROUP + pp.Literal('(').suppress() + ExpressionList + pp.Literal(')').suppress()

# --- Top Level Grammar ---

GroupOrExpression = Group | Expression

Program = GroupOrExpression
