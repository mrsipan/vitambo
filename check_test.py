with open('test/clojure/vitambo/test_all.clj', 'rb') as f:
    data = f.read()
idx = data.find(b'(keyword')
print('Bytes around keyword:', repr(data[idx:idx+30]))
# Check the specific bytes
print('Hex:', ' '.join(f'{b:02x}' for b in data[idx:idx+30]))
# Find the i( case
idx2 = data.find(b'(keyword', idx+1)
print('Bytes around 2nd keyword:', repr(data[idx2:idx2+30]))
print('Hex:', ' '.join(f'{b:02x}' for b in data[idx2:idx2+30]))
