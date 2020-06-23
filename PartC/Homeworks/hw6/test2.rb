require_relative './test1'

class B < A
    def initialize(x, y)
        super(x)
        @y = y
    end

    def m1
        puts @x
        puts @y
    end
end

b = B.new(1, 2)
b.m1
