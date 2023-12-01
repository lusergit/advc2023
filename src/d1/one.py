def firstdigit(line):
    for c in line:
        if c.isdigit():
            return c

def main():
    with open("input.txt") as f:
        summ = 0
        for line in f.readlines():
            d1 = firstdigit(line)
            d2 = firstdigit(reversed(line))
            summ += int(''.join([d1, d2]))
        print("sum: ", summ)

if __name__ == "__main__":
    main()
