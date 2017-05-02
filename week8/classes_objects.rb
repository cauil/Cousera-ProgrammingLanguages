class A
  def m1
    34
  end

  def m2 (x, y)
    z = 34
    if x > y
      false
    else
      x + y + z
    end
  end
end

class B
  def m1
    34
  end

  def m3 x
    x.abs * 2 + self.m1
  end
end
