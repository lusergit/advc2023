import re

def firstdigit(line):
    for c in line:
        if c.isdigit():
            return c

def replace_in_line(line):
    mapp = {
        "one":   "o1e",
        "two":   "t2o",
        "three": "t3e",
        "four":  "4",
        "five":  "5e",
        "six":   "6",
        "seven": "7n",
        "eight": "e8t",
        "nine":  "9e",
    }
    for key in mapp:
        line = line.replace(key, mapp[key])
    return line

def main():
    with open("input.txt") as f:
        summ = 0
        for line in f.readlines():
            line = replace_in_line(line)
            d1 = firstdigit(line)
            d2 = firstdigit(reversed(line))
            summ += int(''.join([d1, d2]))
        print("sum: ", summ)

if __name__ == "__main__":
    main()
