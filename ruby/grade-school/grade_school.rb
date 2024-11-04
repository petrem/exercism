class School

  def add(name, grade)
    if not self.present?(name)
      @roster.push([grade, name])
      @roster.sort!
      true
    else
      false
    end
  end

  def grade(grade)
      @roster
        .filter { |(g, _)| g == grade }
        .map(&:last)
  end

  def present?(student)
    @roster.any? { |(_, s)| s == student }
  end

  def roster
    @roster.map(&:last)
  end
  
  def initialize
    @roster = []
  end
end
