#!/usr/bin/env ruby

undef p

def method_missing(tag, attrs = {})
  str_attrs = attrs.reduce('') { |mem, (k, v)| "#{mem} #{k}=\"#{v}\"" }
  "<#{tag}#{str_attrs}" + (block_given? ? ">\n#{yield}\n</#{tag}>" : " />\n")
end

class Hash
  def to_css
    reduce('') { |mem, (k, v)| "#{mem}#{k}: #{v};" }
  end
end

puts html {
  head {
    title { 'Test page' }
  } +
    body {
    h1(style: {color: :red, 'font-style' => :italic}.to_css) { 'this is a test' } +
    br +
    p { 'This is a test' }
  }
}
