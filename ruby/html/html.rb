#!/usr/bin/env ruby

undef p

def method_missing(tag, attrs = {})
  str_attrs = attrs.reduce('') { |mem, (k, v)| "#{mem} #{k}=\"#{v}\"" }
  "<#{tag}#{str_attrs}" + (block_given? ? ">\n#{yield}\n</#{tag}>" : " />\n")
end

puts html {
  head {
    title { 'Test page' }
  } +
    body {
    h1(color: :red) { 'this is a test' } +
    br +
    p { 'This is a test' }
  }
}
