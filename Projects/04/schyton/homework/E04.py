def snow_white(num_chants, max_sing):
    i = 0
    alternate=["heigh", "ho"]
    result_string = ""
    while i < max_sing:
        i = i + 1
        result_string = result_string + alternate[i%2] + " "
        if i == num_chants:
            result_string = result_string + "its off to work we go "
        if i == max_sing:
            result_string = result_string + "its off to work we go "
    return result_string
print(snow_white(13,35))
