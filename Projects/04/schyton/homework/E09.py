def memo(fn):
    dict_of_values = {}
    def memoized_fn(value):
        if value in dict_of_values:
            return dict_of_values[value]
        else:
            result = fn(value, memoized_fn)
            dict_of_values[value] = result
            return result
    return memoized_fn

def factorial_memo(n, memoized_fn):
    if n == 0 or n == 1:
        return 1
    else:
        return n * memoized_fn(n - 1)
