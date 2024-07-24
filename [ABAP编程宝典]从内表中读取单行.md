## 从内表中读取单行
有三种不同的方法可以指定要读取的行：

1. **通过索引（仅适用于索引表）**
   - 索引表是基于行的顺序进行存储和访问的。每行都有一个唯一的索引。
   - 使用索引可以直接访问内表中的指定行。
2. **通过表键（仅适用于定义了键的表）**
   - 表键可以是主键、排序键或哈希键。表的键定义了行的唯一性和排序方式。
   - 使用表键可以通过键值来读取行。
3. **通过自由键**
   - 自由键允许通过非正式的键值访问内表中的行，通常在哈希表中使用。
   - 这种方法提供了灵活的行访问方式，但要求键值与定义的键相匹配。
## 读取单行时确定目标区域  
在 ABAP 中读取内表的单行时，有三种主要方法来确定目标区域：
### 1. 使用 `INTO` 将行复制到数据对象
使用 `READ TABLE ... INTO` 语句将内表中的一行数据复制到数据对象中。这种方法会将找到的行与数据对象分开。如果修改数据对象或内表中的行，另一个不会受到影响。不过，您可以通过 `MODIFY` 语句基于修改后的数据行来更新内表。
#### 语法：
```abap
READ TABLE itab INTO dobj ...   " dobj 必须具有表的结构类型

READ TABLE itab INTO DATA(dobj_inl) ...

READ TABLE itab INTO ... TRANSPORTING comp1 [comp2 ... ].
```

- `dobj`：数据对象，必须与内表的行类型匹配。
- `dobj_inl`：内联声明的数据对象。
- `TRANSPORTING`：指定要复制的字段。如果不指定，则复制所有字段。
#### 示例：
```abap
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

DATA: itab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 填充内表
APPEND VALUE #( comp1 = 1 comp2 = 'A' ) TO itab.
APPEND VALUE #( comp1 = 2 comp2 = 'B' ) TO itab.
DATA: lv_dobj TYPE ty_line.

" 将第一行读取到数据对象
READ TABLE itab INTO lv_dobj INDEX 1.
IF sy-subrc = 0.
  " 行读取成功
  WRITE: / lv_dobj-comp1, lv_dobj-comp2.
ELSE.
  " 行读取失败
  WRITE: / 'Line not found'.
ENDIF.
```
### 2. 使用 `ASSIGNING` 将行分配给字段符号
使用 `READ TABLE ... ASSIGNING` 将行分配给字段符号。此方法不进行实际的数据复制，修改字段符号即直接修改内表中的行。注意，这种方法不支持 `TRANSPORTING` 附加项，因为整个表都会分配给字段符号。
#### 语法：
```abap
READ TABLE itab ASSIGNING <fs1> ...   " 字段符号必须具有适当的类型。

READ TABLE itab ASSIGNING FIELD-SYMBOL(<fs2>) ...   " 字段符号在内联声明中创建。
```
#### 示例：
```abap
FIELD-SYMBOLS: <fs> TYPE ty_line.

" 将第一行分配给字段符号
READ TABLE itab ASSIGNING <fs> INDEX 1.
IF sy-subrc = 0.
  " 行读取成功
  WRITE: / <fs>-comp1, <fs>-comp2.
  " 修改字段符号会直接影响内表中的行
  <fs>-comp2 = 'Modified'.
ELSE.
  " 行读取失败
  WRITE: / 'Line not found'.
ENDIF.
```
### 3. 使用 `REFERENCE INTO` 将行读取到数据引用变量
使用 `READ TABLE ... REFERENCE INTO` 将行读取到数据引用变量。此方法不进行数据复制。如果要访问该行，必须首先取消引用数据引用。无法使用 `TRANSPORTING` 附加项。
#### 语法：
```abap
READ TABLE itab REFERENCE INTO dref ...

READ TABLE itab REFERENCE INTO DATA(dref_inl) ...
```

- `dref`：数据引用变量，用于引用内表中的行。
- `dref_inl`：内联声明的数据引用变量。
#### 示例：
```abap
DATA: lr_dref TYPE REF TO ty_line.

" 将第一行读取到数据引用变量
READ TABLE itab REFERENCE INTO lr_dref INDEX 1.
IF sy-subrc = 0.
  " 行读取成功
  WRITE: / lr_dref->comp1, lr_dref->comp2.
  " 修改引用的行会直接影响内表中的行
  lr_dref->comp2 = 'Modified'.
ELSE.
  " 行读取失败
  WRITE: / 'Line not found'.
ENDIF.
```
### 总结

- `INTO dobj`：适用于需要将内表行复制到数据对象中，且希望数据对象与内表行分开的场景。注意性能开销。
- `ASSIGNING <fs>`：适用于不希望复制内容而直接修改内表行的场景。字段符号不会复制内容。
- `REFERENCE INTO dref`：适用于需要引用内表行而不进行复制的场景。必须先取消引用数据引用。

以下是通过索引读取内表单行的示例代码和说明：
## 按索引读取单行
在 ABAP 中，通过索引读取内表中的单行可以使用 `READ TABLE` 语句。可以指定索引来读取具体的行，还可以使用 `USING KEY` 附加项来明确指定使用的表键，从而确定读取的行。
### 语法说明：
#### 1. 通过索引读取单行
```abap
DATA: wa TYPE ty_line.

" 通过索引读取内表的单行
READ TABLE itab INTO wa INDEX i.
```

- `itab`：内表。
- `wa`：工作区，用于存储读取的行。
- `i`：索引，指定要读取的行的位置。
#### 2. 使用表键读取单行
```abap
READ TABLE itab INTO wa INDEX i USING KEY primary_key.
```

- `primary_key`：表的主键名称。
#### 3. 使用表表达式读取单行
```abap
" 通过表表达式读取单行，并将结果存储在内联声明的变量中
DATA(lv1) = itab[ 1 ].

TRY.
  DATA(lv2) = itab[ 2 ].
  CATCH cx_sy_itab_line_not_found.
  " 处理行未找到的情况
  ...
ENDTRY.

DATA(lv3) = itab[ KEY primary_key INDEX 3 ].
```

- `itab[ 1 ]`：读取内表中索引为 1 的行。
- `itab[ KEY primary_key INDEX 3 ]`：通过主键索引 3 读取行。
#### 4. 使用构造表达式复制内表行
```abap
" 通过表表达式复制表行，并嵌入到构造表达式中
DATA(lv4) = VALUE #( itab[ 4 ] ).
```

- `VALUE #( itab[ 4 ] )`：读取索引为 4 的行，并使用构造表达式进行处理。
#### 5. 使用数据引用变量读取行
```abap
" 使用 REF 操作符将内表行读取到数据引用变量中
DATA(lv5_ref) = REF #( itab[ 5 ] ).
```

- `REF #( itab[ 5 ] )`：将索引为 5 的行读取到数据引用变量中。
#### 6. 使用 OPTIONAL 或 DEFAULT 处理未找到的行
```abap
" 使用 OPTIONAL 附加项避免读取时抛出异常，读取失败时返回初始值的行
DATA(line1) = VALUE #( itab[ 6 ] OPTIONAL ).

" 使用 DEFAULT 附加项在读取失败时返回默认值
DATA(line2) = VALUE #( itab[ 7 ] DEFAULT itab[ 1 ] ).
```

- `OPTIONAL`：在读取失败时，不抛出异常，而是返回初始值的行。
- `DEFAULT`：在读取失败时，返回指定的默认行（可以是其他表表达式或构造表达式）。
### 说明

1. **按索引读取单行**：
   - 通过指定索引可以读取内表中的特定行。
   - 使用 `USING KEY` 可以通过主键来指定索引。
2. **表表达式**：
   - 表达式 `[ INDEX i ]` 用于直接读取特定索引的行。
   - 使用 `TRY ... CATCH` 块可以处理读取失败的情况。
3. **构造表达式**：
   - 通过 `VALUE #()` 和表表达式结合使用，可以直接将读取的行存储到新变量中。
4. **数据引用变量**：
   - 使用 `REF #()` 将行读取到数据引用变量中，避免了数据复制，直接对行进行操作。
5. **处理未找到的行**：
   - `OPTIONAL` 和 `DEFAULT` 可以用来处理读取失败的情况，避免程序中断或抛出异常。
## ABAP 代码示例
以下是详细的 ABAP 代码示例，展示了如何通过索引、表键、表表达式等方式读取内表中的单行，并处理读取失败的情况。
### 1. 按索引读取单行
```abap
DATA: BEGIN OF ty_line,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 10,
      END OF ty_line.

TYPES: ty_line_tab TYPE STANDARD TABLE OF ty_line WITH NON-UNIQUE KEY comp1.

DATA: itab TYPE ty_line_tab,
      wa   TYPE ty_line.

" 填充内表
itab = VALUE ty_line_tab(
  ( comp1 = 1 comp2 = 'A' )
  ( comp1 = 2 comp2 = 'B' )
  ( comp1 = 3 comp2 = 'C' )
).

" 通过索引读取内表的单行
READ TABLE itab INTO wa INDEX 2.
IF sy-subrc = 0.
  WRITE: / 'Line found:', wa-comp1, wa-comp2.
ELSE.
  WRITE: / 'Line not found'.
ENDIF.
```
### 2. 使用表键读取单行
```abap
" 假设表有主键 comp1
DATA: BEGIN OF ty_line,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 10,
      END OF ty_line.

TYPES: ty_line_tab TYPE STANDARD TABLE OF ty_line WITH UNIQUE KEY comp1.

DATA: itab TYPE ty_line_tab,
      wa   TYPE ty_line.

" 填充内表
itab = VALUE ty_line_tab(
  ( comp1 = 1 comp2 = 'A' )
  ( comp1 = 2 comp2 = 'B' )
  ( comp1 = 3 comp2 = 'C' )
).

" 通过表键读取内表的单行
READ TABLE itab INTO wa WITH KEY comp1 = 2.
IF sy-subrc = 0.
  WRITE: / 'Line found:', wa-comp1, wa-comp2.
ELSE.
  WRITE: / 'Line not found'.
ENDIF.
```
### 3. 使用表表达式读取单行
```abap
DATA: BEGIN OF ty_line,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 10,
      END OF ty_line.

TYPES: ty_line_tab TYPE STANDARD TABLE OF ty_line WITH NON-UNIQUE KEY comp1.

DATA: itab TYPE ty_line_tab,
      lv1  TYPE ty_line,
      lv2  TYPE ty_line,
      lv3  TYPE ty_line,
      lv4  TYPE ty_line,
      lv5_ref TYPE REF TO ty_line.

" 填充内表
itab = VALUE ty_line_tab(
  ( comp1 = 1 comp2 = 'A' )
  ( comp1 = 2 comp2 = 'B' )
  ( comp1 = 3 comp2 = 'C' )
).

" 使用表表达式读取单行
lv1 = itab[ 1 ].

TRY.
  lv2 = itab[ 2 ].
  CATCH cx_sy_itab_line_not_found.
    WRITE: / 'Line 2 not found'.
ENDTRY.

lv3 = itab[ comp1 = 2 ].

" 通过表表达式复制表行，并嵌入到构造表达式中
lv4 = VALUE #( itab[ 1 ] ).

" 使用数据引用变量读取行
lv5_ref = REF #( itab[ 2 ] ).

" 显示结果
WRITE: / 'lv1:', lv1-comp1, lv1-comp2.
WRITE: / 'lv2:', lv2-comp1, lv2-comp2.
WRITE: / 'lv3:', lv3-comp1, lv3-comp2.
WRITE: / 'lv4:', lv4-comp1, lv4-comp2.
IF lv5_ref IS BOUND.
  WRITE: / 'lv5_ref:', lv5_ref->comp1, lv5_ref->comp2.
ENDIF.
```
### 4. 使用 OPTIONAL 或 DEFAULT 处理未找到的行
```abap
DATA: BEGIN OF ty_line,
        comp1 TYPE i,
        comp2 TYPE c LENGTH 10,
      END OF ty_line.

TYPES: ty_line_tab TYPE STANDARD TABLE OF ty_line WITH NON-UNIQUE KEY comp1.

DATA: itab TYPE ty_line_tab,
      line1 TYPE ty_line,
      line2 TYPE ty_line.

" 填充内表
itab = VALUE ty_line_tab(
  ( comp1 = 1 comp2 = 'A' )
  ( comp1 = 2 comp2 = 'B' )
  ( comp1 = 3 comp2 = 'C' )
).

" 使用 OPTIONAL 附加项避免读取时抛出异常，读取失败时返回初始值的行
line1 = VALUE #( itab[ 6 ] OPTIONAL ).
WRITE: / 'line1:', line1-comp1, line1-comp2.

" 使用 DEFAULT 附加项在读取失败时返回默认值
line2 = VALUE #( itab[ 7 ] DEFAULT itab[ 1 ] ).
WRITE: / 'line2:', line2-comp1, line2-comp2.
```
### 代码说明

1. **按索引读取单行**：通过 `READ TABLE ... INDEX i` 语句读取指定索引的行。
2. **使用表键读取单行**：通过 `READ TABLE ... WITH KEY ...` 语句读取匹配表键的行。
3. **表表达式**：
   - 使用 `itab[ i ]` 读取特定索引的行。
   - 使用 `TRY ... CATCH` 处理行未找到的异常情况。
   - 使用 `VALUE #( itab[ i ] )` 直接将行读取到新变量中。
   - 使用 `REF #( itab[ i ] )` 将行读取到数据引用变量中。
4. **处理未找到的行**：
   - `OPTIONAL`：读取失败时返回初始值的行。
   - `DEFAULT`：读取失败时返回指定的默认值行。

这些示例展示了如何灵活地读取内表中的单行数据，并处理各种情况下的读取失败。
## 使用表键读取单行
ABAP内表中可以通过明确指定表键或别名（如果存在）来读取行，下面的示例代码展示了如何使用表键来读取内表中的单行，包括使用主键、次键和别名。
### 示例代码
```abap
TYPES: BEGIN OF struc,
         a TYPE i,
         b TYPE i,
         c TYPE i,
         d TYPE i,
       END OF struc.

DATA: it TYPE SORTED TABLE OF struc
  WITH NON-UNIQUE KEY primary_key ALIAS pk COMPONENTS a b
  WITH NON-UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS c d.

" 填充内表
it = VALUE #( ( a = 1 b = 2 c = 3 d = 4 )
              ( a = 5 b = 6 c = 7 d = 8 )
              ( a = 9 b = 10 c = 11 d = 12 ) ).

DATA: line TYPE struc,
      wa TYPE struc,
      pr_keys TYPE struc,
      sec_keys TYPE struc.

" 使用主键读取表行
line = it[ KEY primary_key COMPONENTS a = 1 b = 2 ].

" 使用主键别名读取表行
line = it[ KEY pk a = 1 b = 2 ].

" 使用次键读取表行
line = it[ KEY sec_key c = 3 d = 4 ].

" 使用次键别名读取表行
line = it[ KEY sk c = 3 d = 4 ].

" 使用 READ TABLE 语句读取表行

" 主键读取
READ TABLE it INTO wa WITH TABLE KEY primary_key COMPONENTS a = 1 b = 2.
IF sy-subrc = 0.
  WRITE: / 'Read with primary key:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with primary key not found'.
ENDIF.

" 主键别名读取
READ TABLE it INTO wa WITH TABLE KEY pk COMPONENTS a = 1 b = 2.
IF sy-subrc = 0.
  WRITE: / 'Read with primary key alias:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with primary key alias not found'.
ENDIF.

" 次键读取
READ TABLE it INTO wa WITH TABLE KEY sec_key COMPONENTS c = 3 d = 4.
IF sy-subrc = 0.
  WRITE: / 'Read with secondary key:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with secondary key not found'.
ENDIF.

" 次键别名读取
READ TABLE it INTO wa WITH TABLE KEY sk COMPONENTS c = 3 d = 4.
IF sy-subrc = 0.
  WRITE: / 'Read with secondary key alias:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with secondary key alias not found'.
ENDIF.

" 基于工作区中的键值读取表行

" 填充工作区
pr_keys = VALUE struc( a = 1 b = 2 ).
sec_keys = VALUE struc( c = 3 d = 4 ).

" 从主键工作区读取
READ TABLE it FROM pr_keys INTO wa.
IF sy-subrc = 0.
  WRITE: / 'Read from primary keys:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with primary keys not found'.
ENDIF.

" 使用主键读取
READ TABLE it FROM pr_keys USING KEY primary_key INTO wa.
IF sy-subrc = 0.
  WRITE: / 'Read from primary key using key:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with primary key using key not found'.
ENDIF.

" 使用次键读取
READ TABLE it FROM sec_keys USING KEY sec_key INTO wa.
IF sy-subrc = 0.
  WRITE: / 'Read from secondary keys using key:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with secondary keys using key not found'.
ENDIF.

" 使用次键别名读取
READ TABLE it FROM sec_keys USING KEY sk INTO wa.
IF sy-subrc = 0.
  WRITE: / 'Read from secondary key alias using key:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with secondary key alias using key not found'.
ENDIF.
```
### 代码解释

1. **定义结构和内表类型**：首先定义了结构类型 `struc`，然后定义了一个排序内表 `it`，该内表具有主键 `primary_key` 和次键 `sec_key`，并为每个键定义了别名 `pk` 和 `sk`。
2. **填充内表**：使用 `VALUE` 语句填充内表 `it`。
3. **通过表表达式读取表行**：展示了如何使用主键、次键及其别名通过表表达式读取表行。
4. **使用 **`READ TABLE`** 语句读取表行**：展示了如何使用 `READ TABLE` 语句通过主键、次键及其别名读取表行。
5. **基于工作区中的键值读取表行**：展示了如何使用工作区中的键值读取表行。
### 代码结果
![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1721272636976-ca3c8407-7c2d-40af-9ff0-0e0543ac9613.png#averageHue=%23f9f9f9&clientId=ube55fc59-1f67-4&from=paste&height=250&id=u7cd438b1&originHeight=312&originWidth=915&originalType=binary&ratio=1.25&rotation=0&showTitle=false&size=22771&status=done&style=none&taskId=u61c973be-1c6d-4af7-ac65-27df7102316&title=&width=732)

## 使用自由键读取单行
使用自由键读取单行时，指定的组件用作键，不必是表键的一部分。
以下是使用自由键读取单行的示例代码，包括如何使用二进制搜索优化读取访问。
#### 示例代码
```abap
TYPES: BEGIN OF struc,
         a TYPE i,
         b TYPE i,
         c TYPE i,
         d TYPE i,
       END OF struc.

DATA: it TYPE HASHED TABLE OF struc
  WITH UNIQUE  KEY primary_key ALIAS pk COMPONENTS a b
  WITH UNIQUE SORTED KEY sec_key ALIAS sk COMPONENTS c d.

" 定义排序表类型
DATA: it1 TYPE SORTED TABLE OF struc WITH UNIQUE KEY b.

" 填充内表
it = VALUE #( ( a = 1 b = 2 c = 3 d = 4 )
              ( a = 5 b = 6 c = 7 d = 8 )
              ( a = 9 b = 10 c = 11 d = 12 ) ).

IT1[] = IT[].

DATA: line TYPE struc,
      wa TYPE struc.

" 使用自由键读取表行
line = it[ b = 2 ].

" 使用 READ TABLE 语句和自由键读取表行
READ TABLE it INTO wa WITH KEY b = 2.
IF sy-subrc = 0.
  WRITE: / 'Read with free key:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with free key not found'.
ENDIF.

" 使用二进制搜索优化读取访问
READ TABLE it1 INTO wa WITH KEY b = 2 BINARY SEARCH.
IF sy-subrc = 0.
  WRITE: / 'Read with free key using binary search:', wa-a, wa-b, wa-c, wa-d.
ELSE.
  WRITE: / 'Line with free key using binary search not found'.
ENDIF.
```
### 代码解释

1. **定义结构和内表类型**：首先定义了结构类型 `struc`，然后定义了一个排序内表 `it`，该内表具有主键 `primary_key` 和次键 `sec_key`，并为每个键定义了别名 `pk` 和 `sk`。
2. **填充内表**：使用 `VALUE` 语句填充内表 `it`。
3. **通过自由键读取表行**：展示了如何使用自由键 `b = 2` 通过表表达式读取表行。
4. **使用 **`READ TABLE`** 语句和自由键读取表行**：展示了如何使用 `READ TABLE` 语句和自由键读取表行，并检查 `sy-subrc` 以确定读取是否成功。
5. **使用二进制搜索优化读取访问**：展示了如何使用 `READ TABLE` 语句和自由键结合 `BINARY SEARCH` 优化读取访问。`BINARY SEARCH` 可以提高读取速度，特别是在大数据量时。
### 注意事项

- 哈希表不支持索引操作，因此在读取哈希表中的数据时，不能使用显式或隐式索引。
- 哈希表也不支持 `BINARY SEARCH`，因此不能在哈希表上使用二进制搜索来优化读取访问。
- 在排序表或索引表中使用 `BINARY SEARCH` 时，指定的键必须是表键的初始部分。
