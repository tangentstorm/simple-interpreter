-- this is just a little test framework.
-- it runs code/*.si and send the output to the corresponding code/*.out file.
-- then it runs `git status code` so you can see what's changed.
-- (the idea is to keep correct output in git)

require 'lfs'


re='^.*[.]si$' -- match *.si
for f in lfs.dir('code') do
   if f:match(re) then
      base = f:sub(1,-4) -- everything but last 3 chars.
      print('running example: ' .. base)
      os.execute('./simp < code/' .. base .. '.si > code/' .. base .. '.out')
   end
end

os.execute('git status code')
