class Foo
    def initialize(max)
        @max = max
    end
    
    def silly
        yield(4, 5) + yield(@max, @max)
    end

    def count(base)
        if base > @max
            raise "Reach the max"
        elsif yield base
            1
        else 
            1 + (count(base+1) {|x| yield x})
        end
    end
end

f = Foo.new(1000)

puts f.silly {|a,b| 2*a - b}

puts f.count(10) {|i| (i * i) == (34 * i)}