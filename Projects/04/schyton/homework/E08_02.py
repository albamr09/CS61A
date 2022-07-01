
def decode_cipher(letter_dictionary, coded_message):
    decoded_message = ""
    for char in coded_message:
        if char != ' ':
            decoded_message = decoded_message + letter_dictionary[char]
        else: 
            decoded_message = decoded_message + " "
    return decoded_message
