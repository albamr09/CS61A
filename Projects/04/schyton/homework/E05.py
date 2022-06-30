def push_first_odd_back(lst):
    n = len(lst)
    len_limit = n - 1
    first_odd = -1
    for i in range(n):
        if first_odd == -1:
            if lst[i] % 2 != 0:
                first_odd = lst[i]
                lst[i] = lst[i + 1]
        elif i == len_limit:
            lst[n - 1] = first_odd
        else:
            lst[i] = lst[i + 1]
