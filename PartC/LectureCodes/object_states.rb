class A

    My_age = 22

    def initialize(f = 1)
        @foo = f
        @@bar = 0
    end

    def m1 (x)
        @foo += x
        @@bar += 1
    end

    def reset_foo(f=0)
        @foo = f
    end

    def foo
        @foo
    end

    def bar
        @@bar
    end
    
end
