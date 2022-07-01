def rotate_letters(offset):
    letter_list = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    n = len(letter_list)
    letter_dictionary = {}
    for i in range(n):
       letter_dictionary[letter_list[i]] = letter_list[(i + offset)%n]
    return letter_dictionary
