class School

  def add(name, grade)
    if not self.present?(name)
      @roster.push([grade, name])
      true
    else
      false
    end
  end

  def grade(grade)
      @roster
        .filter { |(g, _)| g == grade }
        .map { |(_, s)| s }
        .sort
  end

  def present?(student)
    @roster.any? { |(_, s)| s == student }
  end

  def roster
    @roster.sort.map { |(_, s)| s }
  end
  
  def initialize
    @roster = []
  end
end

s = School.new
s.add("zuzu", 2)
s.add("zuzu", 3)
s.grade(2)
