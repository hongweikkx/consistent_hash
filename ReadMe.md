## 一致性hash算法实现

### how to use
1. make && make run
2. 
```
   1> R1 = consistent_hash:new().
   []
   2> R2 = consistent_hash:add_node("127.0.0.1:8000", R1).
   [{1733494364,{node,1733494364,"127.0.0.1:8000",false}}]
   
   3> R3 = consistent_hash:add_node("128.3.3.4:9000", R2).
   [{1263834227,{node,1263834227,"128.3.3.4:9000",false}},
    {1733494364,{node,1733494364,"127.0.0.1:8000",false}}]
    
   4> R4 = consistent_hash:add_node("233.3.4.7:735", R3).
   [{1263834227,{node,1263834227,"128.3.3.4:9000",false}},
    {1733494364,{node,1733494364,"127.0.0.1:8000",false}},
    {2642162750,{node,2642162750,"233.3.4.7:735",false}}]
    
   5> K = "test".
   "test"
   
   6> consistent_hash:select_node(K, R4).
   "128.3.3.4:9000"
   
   7> R5 = consistent_hash:del_node("128.3.3.4:9000", R4).
    [{1733494364,{node,1733494364,"127.0.0.1:8000",false}},
     {2642162750,{node,2642162750,"233.3.4.7:735",false}}]
     
   8> consistent_hash:select_node(K, R5).
   "127.0.0.1:8000"
   
```

