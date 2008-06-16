class MyKlass
  def method_name
    "double-quoted" and :symbol
    'single-quoted'
    %Q(q-style)
    <<EOS
heredoc
EOS
  end
end
