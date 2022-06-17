def memoize(func):
    memos = {}
    def memo(x):
        if x in memos:
            return memos[x]
        else:
            t = func(x, memo)
            memos[x] = t
            return t
    return memo
