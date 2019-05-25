def count_change(amount)
  cc(amount, 5, 0)
end

def cc(amount, kinds_of_coins, depth)
  if amount == 0
    indent = depth > 0 ? '| ' * (depth - 1) : ''
    puts "#{indent}\033[97;41m\u{2191}1\033[0m"
    1
  elsif amount < 0 || kinds_of_coins == 0
    indent = '| ' * (depth - 1)
    puts "#{indent}\033[90m\u{2191}0\033[0m"
    0
  else
    indent = '| ' * depth
    puts "#{indent}cc(#{amount}, #{kinds_of_coins - 1})"
    x = cc(amount, kinds_of_coins - 1, depth + 1)
    puts "#{indent}cc(#{amount - first_denomination(kinds_of_coins)}, #{kinds_of_coins})"
    y = cc(amount - first_denomination(kinds_of_coins), kinds_of_coins, depth + 1)
    x + y
  end
end

DENOMINATIONS = [1, 5, 10, 25, 50]

def first_denomination(kinds_of_coins)
  DENOMINATIONS[kinds_of_coins - 1]
end

result = count_change(ARGV[0].to_i)

puts
puts result
