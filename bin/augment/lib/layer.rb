class Layer
  attr_accessor :lines, :color, :message

  @layers = Hash.new([])
  
  def self.layers; @layers; end
  
  def initialize(file, lines, color, message)
    @file, @lines, @color, @message = [file, lines, color, message]
    self.class.layers[file] ||= []
    self.class.layers[file] << self
  end

  def render(string)
    send("render_#{Options[:output]}", string)
  end
  
  def render_ansi(string)
    string.colorize_lines(@lines.to_a, @color)
  end
  
  def render_emacs
    "(save-excursion
  (goto-line #{@lines.first})
  (let ((overlay-start (line-beginning-position)))
    (goto-line #{@lines.last})
    (let ((overlay (make-overlay overlay-start (line-end-position)) ))
    (overlay-put overlay 'face (background-color . #{@color}))
    (overlay-put overlay 'message \"#{@message}\"))))"
  end
end
