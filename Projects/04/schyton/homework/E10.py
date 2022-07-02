def gen_exp(n):
    i = -1
    while True:
        i = i + 1
        yield n**(i)

gen_exp_to_7 = gen_exp(2)
for i in range(7):
    print(next(gen_exp_to_7))
