class A

    def foo=(x)
        @foo = x
    end

    def foo
        @foo
    end

    def m2(x)
        m1(x)
    end

    private 
    def m1(x)
        @foo += x
    end

end