def f(nums):
    return [num+1 for num in nums]

if __name__ == "__main__":
    data = [1,2,3,4,5]
    result = ",".join([str(item) for item in f(data)])
    with open("new_file", "w") as f:
        f.write(result)
