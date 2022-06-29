def fizzbuzz(n):
    result_string=""
    for i in range(n):
        i=i+1
        if i%15==0:
            result_string=result_string+" FizzBuzz"
        elif i%3==0:
            result_string=result_string+" Fizz"
        elif i%5==0:
            result_string=result_string+" Buzz"
    return result_string
