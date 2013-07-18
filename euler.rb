def make(&func)
  Enumerator.new do |yielder|
    n = 1;
    loop do
      yielder.yield func.call(n)
      n+=1
    end
  end
end

def getNumbers(&func)
  (make &func).take_while { |v| v < 10000 }.drop_while {|v| v < 1000}.map { |v| v.to_s}
end

#triangles = getNumbers {|n| n*(n+1)/2}
#squares = getNumbers {|n| n**2}
#pentagonals = getNumbers {|n| n*(3*n-1)/2}
#hexagonals = getNumbers {|n| n*(2*n-1)}
#heptagonals = getNumbers {|n| n*(5*n-3)/2}
#octagonals = getNumbers {|n| n*(3*n-2)}
#

def prime(n)
  return false if (n % 2 == 0 or n < 2)
  top = Math.sqrt(n).floor

  (3..top).each do |i|
    return false if n % i == 0 
  end

  return true
end

def perfect_square(n)
  sqrt = Math::sqrt(n) 
  return (n == sqrt.floor**2) 
end


def what_power(k)
  nom = 9
  i = 1

  return 0 if (k % 2 == 0) || (k % 5 == 0)

  while true
    return i if(nom % k == 0)
    nom += 9*(10**i)
    i += 1
  end
  
end

def solve26()
  dict = {}
  (2..1000).each do |i|
    dict[i] = what_power(i)
  end

  puts dict.max{|a,b| a[1] <=> b[1]}[0]

end



def solve50()
  primes = (2..10000).find_all { |i| prime(i) }

  prime_sums = [2]
  (1..primes.length - 1).each do |k|
    prime_sums[k] = prime_sums[k-1] + primes[k-1]
  end
  
  l = prime_sums.length

  p = []

  (0..l-1).each do |a|
    p[a] = []
    p[a][a] = primes[a]
    p[0][a] = prime_sums[a]
  end

  (1..l-1).each do |a|
    (l-1).downto(a) do |b|
      p[a][b] = prime_sums[b] - prime_sums[a-1]
    end
  end


  max = 2
  a = 0
  b = 0
  max_diff = 0

  p.each_with_index do |p1,i1|
    p1.each_with_index do |p2,i2|
      if p2 != nil && p2 < 1000000 && (i2-i1) > max_diff && prime(p2)
        max = p2
        a = i1
        b = i2
        max_diff = (b-a)
      end
    end
  end

  puts max, a,b

end

solve50()



def solve46()
  primes = (2..6000).find_all{ |i| prime(i) }

  (9..10000).step(2).each do |i|
    poss = false

    next if prime(i)        # skip if i is prime

    primes.each do |p|
      break if p > i

      diff = (i - p)/2
    
      if perfect_square(diff)
        k = Math.sqrt(diff)
        #puts "#{i} = #{p} + 2 x #{k} ^ 2"
        poss = true
        break
      end
    end

    if(!poss)
      puts i
      exit
    end

  end
end




