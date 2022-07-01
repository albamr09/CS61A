# state 
# 0 - nothing found
# 1 - letter c found
# 2 - letter a found
# ACCEPT - letter t found <<
# 3 - letter d found
# 4 - letter o found
# ACCEPT - letter t found <<
def cat_dog(str):
    state = 0
    count_cat = 0
    count_dog = 0
    for char in str:
        if state == 0:
            if char == 'c':
                state = 1
            elif char == 'd':
                state = 3
            else:
                state = 0
        elif state == 1 and char == 'a':
            state = 2
        elif state == 2 and char == 't':
            count_cat = count_cat + 1
            state = 0
        elif state == 3 and char == 'o':
            state = 4
        elif state == 4 and char == 'g':
            count_dog = count_dog + 1
            state = 0
        else:
            state = 0
    return count_cat == count_dog
