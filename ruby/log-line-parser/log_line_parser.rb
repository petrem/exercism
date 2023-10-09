class LogLineParser
  def initialize(line)
    @line = line
    @parsed = _parse
  end

  def message
    @parsed["message"].strip
  end

  def log_level
    @parsed["level"].downcase
  end

  def reformat
    "#{message} (#{log_level.downcase})"
  end

  LOG_FORMAT_RE = /\[(INFO|ERROR|WARNING)\]:\s+(.*)$/

  def _parse
    if (match = @line.match(LOG_FORMAT_RE)) then
      {
        "level"   => match[1],
        "message" => match[2],
      }
    else
      raise "Invalid log line: #{@line}"
    end
  end
end
