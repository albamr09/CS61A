def char_freq(str):
    frequency_dictionary = {}
    for char in str:
        if char not in frequency_dictionary:
            frequency_dictionary[char] = 1
        else:
            frequency_dictionary[char] = frequency_dictionary[char] + 1
    return frequency_dictionary
