class Test
    def initialize(foo, bar)
        @foo = foo
        @bar = bar
    end
    def double
        @foo = @foo + @foo
        @bar = @bar + @bar
    end
    def foo
        @foo
    end
    def foo=(x)
        @foo = x
    end
end
    