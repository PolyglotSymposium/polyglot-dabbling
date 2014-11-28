#!/usr/bin/env ruby

undef p

def method_missing(name, attrs = {})
  str_attrs = attrs.map { |k, v| "#{k}=\"#{v}\"" }.join(' ')
  "<#{name}#{str_attrs.empty? ? '' : ' ' + str_attrs }>#{block_given? ? yield : ''}</#{name}>"
end

puts html {
  h1(color: :red) { 'this is a test' } + br +
  p { 'This is a test' }
}
