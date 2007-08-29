class Layer
  attr_accessor :lines, :color, :message

  @layers = {}
  
  def self.layers; @layers; end
  
  def initialize(file, lines, color, message)
    @file, @lines, @color, @message = [file, lines, color, message]
    self.class.layers[file] ||= []
    self.class.layers[file] << self
  end

  def render(string)
    # by default render to ANSI color
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
