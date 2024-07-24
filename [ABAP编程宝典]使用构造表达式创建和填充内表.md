ABAP 提供了各种构造表达式，使得创建和填充内表更加简洁和灵活。 <br />构造函数表达式几乎可以在 ABAP 中的各种位置/语句中指定。
# `VALUE` 操作符
在以下示例中，内表通过赋值一个使用 `VALUE` 操作符内联构造的内表来填充。这个内联构造的表包含两行。其中一行使用已有的与行类型兼容的结构 `line`，另一行则是内联构造的。
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 定义一个已有的行结构
DATA(line) = VALUE ty_line( comp1 = 1 comp2 = 'A' ).

" 使用 VALUE 操作符创建并填充内表
DATA(itab) = VALUE ty_line_tab(
               ( line )        " 使用已有的结构
               ( comp1 = 2 comp2 = 'B' ) " 内联构造行
             ).
```
在这个示例中：

1. 我们首先定义了一个结构类型 `ty_line`，包含两个组件 `comp1` 和 `comp2`。
2. 然后，我们定义了一个内表类型 `ty_line_tab`。
3. 接着，我们创建了一个名为 `line` 的行结构，并为其赋值。
4. 最后，我们使用 `VALUE` 操作符创建并填充内表 `itab`，其中包含两行：一行是使用已有的结构 `line`，另一行是内联构造的。

这样，内表 `itab` 将包含两行，分别是通过已有结构和内联构造来填充的。
##  通过内联声明和构造表达式来创建和填充内表  
在ABAP中，内表的创建和使用可以通过不同的方式来完成。以下是关于内表类型、内联声明和字符串表的示例和说明：
### 示例代码
```abap
" 内表类型定义
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

TYPES: it_type TYPE TABLE OF ty_line WITH EMPTY KEY.

" 内联声明并填充内表
DATA(it_in) = VALUE it_type(
                ( comp1 = 1 comp2 = 'A' )  " 第一行
                ( comp1 = 2 comp2 = 'B' )  " 第二行
              ).

" 创建字符串表（string table）
DATA(str_tab_a) = VALUE string_table( ( `Hello` ) ( `World` ) ).  " 填充字符串表

DATA(str_tab_b) = VALUE string_table( ).  " 创建一个初始的空字符串表

DATA str_tab_c TYPE string_table.  " 声明一个空字符串表，与上一行的效果相同
```
### 说明

1. **内表类型定义**:
   - 我们首先定义了一个结构类型 `ty_line`，包含了两个字段 `comp1` 和 `comp2`。
   - 然后，我们使用 `TYPES` 关键字定义了一个内表类型 `it_type`，其元素类型为 `ty_line`。
2. **内联声明和填充内表**:
   - 使用 `VALUE` 操作符，我们声明了一个内表 `it_in`，类型为 `it_type`，并初始化了两行数据。
   - 每行数据使用了结构 `ty_line` 的字段进行填充。
3. **创建字符串表**:
   - 使用 `VALUE` 操作符创建了一个字符串表 `str_tab_a`，并填充了两个字符串元素 `"Hello"` 和 `"World"`。
   - `str_tab_b` 也是通过 `VALUE` 操作符创建的空字符串表。
   - `str_tab_c` 是通过类型声明创建的空字符串表，与使用 `VALUE` 操作符创建空字符串表的效果相同。
### 注意事项

- 在使用 `VALUE` 操作符时，需要确保填充的内容与声明的数据类型相匹配。
- 内联声明和填充内表可以使代码更加简洁和易于理解，特别适用于需要初始化数据的情况。
- 字符串表可以方便地存储和处理字符串数据，可以用于各种文本处理和输出场景。
- 在使用 `VALUE` 操作符时，操作的内表必须指定键，可以指定`EMPTY KEY`或任意组件为键值。

以上示例展示了如何在ABAP中定义和使用内表类型、进行内联声明，并创建字符串表的方法。
## 使用 BASE 附加添加新行
在使用 `VALUE` 操作符对内表进行赋值时，会初始化内表并删除现有内容。如果你希望在不删除现有内容的情况下添加新行，可以使用 `BASE` 附加。<br />下面是如何使用 `BASE` 附加来实现这一点的示例：
### 示例 - 使用 BASE 附加添加新行
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义具体的内表类型
TYPES: ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 创建并填充内表
DATA(itab) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' )  " 第一行
                ( comp1 = 2 comp2 = 'B' )  " 第二行
              ).

" 使用 BASE 附加添加新行
itab = VALUE ty_line_tab(
         BASE itab
         ( comp1 = 3 comp2 = 'C' )  " 第三行
         ( comp1 = 4 comp2 = 'D' )  " 第四行
       ).

cl_demo_output=>DISPLAY( itab ).
```
在上述代码中，`BASE itab` 保留了 `itab` 中现有的内容，并在其基础上添加了新行。<br />![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720761506607-0d7fdae3-cb81-4cf1-8851-2d0670118160.png#averageHue=%23fefdfd&clientId=uf56f9938-197d-4&from=paste&height=256&id=ufd56a79b&originHeight=256&originWidth=811&originalType=binary&ratio=1&rotation=0&showTitle=false&size=3933&status=done&style=none&taskId=u52a8f499-a20d-466f-8b1d-7f9b03fbcd2&title=&width=811)
### 详细说明

1. **定义结构类型**：定义一个包含两个字段的结构类型 `ty_line`。
2. **定义内表类型**：定义一个具体的内表类型 `ty_line_tab`。
3. **创建并填充内表**：使用 `VALUE` 操作符初始化内表 `itab` 并填充两行数据。
4. **使用 BASE 附加添加新行**：
   - 使用 `BASE itab` 保留现有的内容。
   - 添加新行 `( comp1 = 3 comp2 = 'C' )` 和 `( comp1 = 4 comp2 = 'D' )`。
### 示例 - 结合使用 BASE 和内联声明
如果你希望在内联声明时使用 `BASE` 附加，也可以这样做：
```abap

" 使用 BASE 附加和内联声明添加新行
DATA(itab_inline) = VALUE ty_line_tab(
                      BASE itab
                      ( comp1 = 5 comp2 = 'E' )  " 第五行
                      ( comp1 = 6 comp2 = 'F' )  " 第六行
                    ).
cl_demo_output=>DISPLAY( itab_inline ).
```
通过使用 `BASE` 附加，你可以在不删除现有内容的情况下将新行添加到内表中。这在处理动态数据集时非常有用。<br />![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720761682294-02369a7d-12a5-40d0-b82e-d414bdd915e6.png#averageHue=%23fcfcfc&clientId=uf56f9938-197d-4&from=paste&height=269&id=u1b43e8b7&originHeight=269&originWidth=446&originalType=binary&ratio=1&rotation=0&showTitle=false&size=4009&status=done&style=none&taskId=u293a4c4d-d05d-4eae-bc7d-3ff97611056&title=&width=446)
### 总结

- **初始化和删除现有内容**：直接使用 `VALUE` 操作符会初始化内表并删除现有内容。
- **保留现有内容并添加新行**：使用 `BASE` 附加可以在保留现有内容的基础上添加新行。
- **内联声明**：结合 `BASE` 和内联声明可以简化代码编写。

这种方法确保了在添加新行时，内表中的现有数据不会被删除。
## 使用 LINES OF 附加项
在ABAP中，可以使用`VALUE`操作符的`LINES OF`附加项从另一个内表中添加行到当前内表。这可以有效地将一个内表的所有行复制到另一个内表中。以下是如何使用`LINES OF`附加项的示例：
### 示例 - 使用 LINES OF 附加项
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 创建并填充两个内表
DATA(itab1) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' )
                ( comp1 = 2 comp2 = 'B' )
             ).

DATA(itab2) = VALUE ty_line_tab(
                ( comp1 = 3 comp2 = 'C' )
                ( comp1 = 4 comp2 = 'D' )
             ).

" 使用 VALUE 操作符和 LINES OF 附加项创建并填充内表
itab1 = VALUE #(
          BASE itab1
          ( comp1 = 5 comp2 = 'E' )
          ( comp1 = 6 comp2 = 'F' )
          ( LINES OF itab2 )
        ).

" 显示结果
cl_demo_output=>display( itab1 ).

```
### 解释

1. **定义结构类型**：ty_line结构包含两个组件comp1和comp2。
2. **定义内表类型**：ty_line_tab类型是由结构ty_line构成的内表，使用EMPTY KEY确保表没有键。
3. **创建并填充两个内表**：itab1和itab2分别用初始数据填充。
4. **使用VALUE操作符和LINES OF附加项填充内表**：
   - 使用BASE itab1保留itab1的现有内容。
   - 添加新的行。
   - 使用LINES OF itab2将itab2的所有行添加到itab1中。
5. **显示结果**：cl_demo_output=>display用于显示itab1的内容。
### 注意事项

- **BASE 附加项**：`BASE`附加项用于保留`itab1`的现有内容。
- **LINES OF 附加项**：`LINES OF itab2`用于将`itab2`的所有行添加到`itab1`。

通过这种方法，你可以轻松地将一个内表的所有行复制到另一个内表中，并且保留现有的内容。
# CORRESPONDING 操作符
## 使用`CORRESPONDING`操作符来复制另一个内部表
在ABAP中，可以使用`CORRESPONDING`操作符来复制另一个内部表的内容。该操作符会根据字段名匹配将源表中的数据复制到目标表中。以下是一个示例，展示如何使用`CORRESPONDING`操作符复制内部表的内容：
### 示例-`CORRESPONDING`
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 创建并填充第一个内表
DATA(itab1) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' )
                ( comp1 = 2 comp2 = 'B' )
             ).

" 创建并填充第二个内表
DATA(itab2) = VALUE ty_line_tab(
                ( comp1 = 3 comp2 = 'C' )
                ( comp1 = 4 comp2 = 'D' )
             ).

" 使用 CORRESPONDING 操作符复制内部表的内容
itab1 = CORRESPONDING ty_line_tab( itab2 ).

" 显示结果
cl_demo_output=>display( itab1 ).
```
### 解释

1. **定义结构类型**：`ty_line`结构包含两个组件`comp1`和`comp2`。
2. **定义内表类型**：`ty_line_tab`类型是由结构`ty_line`构成的内表，使用`EMPTY KEY`确保表没有键。
3. **创建并填充第一个内表**：`itab1`用初始数据填充。
4. **创建并填充第二个内表**：`itab2`用初始数据填充。
5. **使用**`CORRESPONDING`**操作符复制内部表的内容**：`itab1 = CORRESPONDING ty_line_tab( itab2 )`将`itab2`的内容复制到`itab1`中，匹配字段名。
6. **显示结果**：`cl_demo_output=>display`用于显示`itab1`的内容。

运行此代码后，`itab1`将包含`itab2`的所有行，输出如下：

- ![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720764033014-26c6a624-3184-45d0-a07a-930e6b6e4bf1.png#averageHue=%23fdfdfd&clientId=uffae9d7d-2942-4&from=paste&height=215&id=ud610fa14&originHeight=215&originWidth=567&originalType=binary&ratio=1&rotation=0&showTitle=false&size=2920&status=done&style=none&taskId=u590ed3ab-2b3f-48ec-9006-50eeb2dcab3&title=&width=567)


在ABAP中，使用`MOVE-CORRESPONDING`操作符可以复制另一个内部表的内容，并使用`KEEPING TARGET LINES`选项保留目标表中现有的内容。以下示例演示了如何实现这一点：
### 示例-`KEEPING TARGET LINES`
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 创建并填充第一个内表
DATA(itab1) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' )
                ( comp1 = 2 comp2 = 'B' )
             ).

" 创建并填充第二个内表
DATA(itab2) = VALUE ty_line_tab(
                ( comp1 = 3 comp2 = 'C' )
                ( comp1 = 4 comp2 = 'D' )
             ).

" 使用 MOVE-CORRESPONDING 操作符和 KEEPING TARGET LINES 选项复制内部表的内容
MOVE-CORRESPONDING itab2 to itab1 KEEPING TARGET LINES.

" 显示结果
cl_demo_output=>display( itab1 ).
```
### 解释

1. **定义结构类型**：`ty_line`结构包含两个组件`comp1`和`comp2`。
2. **定义内表类型**：`ty_line_tab`类型是由结构`ty_line`构成的内表，使用`EMPTY KEY`确保表没有键。
3. **创建并填充第一个内表**：`itab1`用初始数据填充。
4. **创建并填充第二个内表**：`itab2`用初始数据填充。
5. **使用**`CORRESPONDING`**操作符和**`KEEPING TARGET LINES`**选项复制内部表的内容**：`itab1 = VALUE #( BASE itab1 ( LINES OF itab2 ) )`将`itab2`的内容复制到`itab1`中，同时保留`itab1`中现有的内容。
6. **显示结果**：`cl_demo_output=>display`用于显示`itab1`的内容。

运行此代码后，`itab1`将包含以下内容：

- ![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720765874246-e189e6f9-8e18-4522-8ab1-904f10b2c0f3.png#averageHue=%23fcfcfb&clientId=uffae9d7d-2942-4&from=paste&height=218&id=u95a5b1db&originHeight=218&originWidth=354&originalType=binary&ratio=1&rotation=0&showTitle=false&size=3080&status=done&style=none&taskId=u214022e2-d352-45ea-9165-c0d655cb91a&title=&width=354)
## `CORRESPONDING`操作符通过  `MAPPING`  关系将组件赋值到另一个内表
在ABAP中，可以使用`CORRESPONDING`操作符通过`MAPPING` 映射关系将组件赋值到另一个内表。这在源表和目标表的结构不同的情况下尤其有用。
### 示例-`MAPPING` 
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义一个不同的结构类型
TYPES: BEGIN OF ty_line_diff,
         field1 TYPE i,
         field2 TYPE c LENGTH 10,
       END OF ty_line_diff.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.
TYPES ty_line_diff_tab TYPE TABLE OF ty_line_diff WITH EMPTY KEY.

" 创建并填充具有初始数据的内表
DATA(itab1) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' )
                ( comp1 = 2 comp2 = 'B' )
             ).

" 创建并填充具有不同结构的内表
DATA(itab_diff) = VALUE ty_line_diff_tab(
                    ( field1 = 3 field2 = 'C' )
                    ( field1 = 4 field2 = 'D' )
                 ).

" 使用 CORRESPONDING 操作符和 MAPPING 选项复制内容，同时保留现有内容
itab1 = VALUE #( BASE itab1
                 ( LINES OF CORRESPONDING ty_line_tab( itab_diff MAPPING
                                     comp1 = field1
                                     comp2 = field2 ) ) ).

" 显示结果
cl_demo_output=>display( itab1 ).
```
### 解释

1. **定义结构类型**：定义两个结构类型`ty_line`和`ty_line_diff`，它们的组件名称不同。
2. **定义内表类型**：`ty_line_tab`和`ty_line_diff_tab`分别是由`ty_line`和`ty_line_diff`构成的内表。
3. **创建并填充第一个内表**：`itab1`用初始数据填充。
4. **创建并填充具有不同结构的内表**：`itab_diff`用初始数据填充。
5. **使用**`CORRESPONDING`**操作符和**`MAPPING`**选项复制内容**：`itab1 = VALUE #( BASE itab1 ( LINES OF CORRESPONDING ty_line_tab( itab_diff MAPPING comp1 = field1 comp2 = field2 ) ) )`将`itab_diff`的内容复制到`itab1`中，同时处理字段名的映射，并保留`itab1`中现有的内容。
6. **显示结果**：`cl_demo_output=>display`用于显示`itab1`的内容。

运行此代码后，`itab1`将包含以下内容：

- ![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720766144752-6f4b7065-3aff-47ec-9c38-be5e0ae96543.png#averageHue=%23fcfcfb&clientId=uffae9d7d-2942-4&from=paste&height=234&id=uc55de9d7&originHeight=234&originWidth=348&originalType=binary&ratio=1&rotation=0&showTitle=false&size=3101&status=done&style=none&taskId=ue35a1b93-3c38-4159-ab3a-4373b87ee15&title=&width=348)
### 注意事项

- **BASE**：使用`BASE`选项可以保留目标内表中的现有内容。
- **LINES OF**：使用`LINES OF`可以将另一个内表的行添加到目标内表中。
- **MAPPING**：`MAPPING`选项用于处理字段名的不同，使得不同结构的内表可以互相复制内容。

这种方法使得在处理具有不同结构的内表时非常灵活和高效。
## 使用`CORRESPONDING`操作符的`EXCEPT`附加选项排除某些组件
 在ABAP中，可以使用`CORRESPONDING`操作符的`EXCEPT`附加选项排除某些组件，从而将一个内表的内容复制到另一个内表时排除不需要的字段。  
### 示例-`CORRESPONDING`操作符的`EXCEPT`
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
         comp3 TYPE c LENGTH 5,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE TABLE OF ty_line WITH EMPTY KEY.

" 创建并填充具有初始数据的内表
DATA(itab1) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' comp3 = 'X' )
                ( comp1 = 2 comp2 = 'B' comp3 = 'Y' )
             ).

" 创建并填充具有不同结构的内表
DATA(itab2) = VALUE ty_line_tab(
                ( comp1 = 3 comp2 = 'C' comp3 = 'Z' )
                ( comp1 = 4 comp2 = 'D' comp3 = 'W' )
             ).

" 使用 CORRESPONDING 操作符和 EXCEPT 选项复制内容，同时排除某些字段
itab1 = VALUE #( BASE itab1
                 ( LINES OF CORRESPONDING #( itab2 EXCEPT comp3 ) ) ).

" 显示结果
cl_demo_output=>display( itab1 ).
```
### 解释

1. **定义结构类型**：定义结构类型`ty_line`，其中包含三个组件`comp1`、`comp2`和`comp3`。
2. **定义内表类型**：定义内表类型`ty_line_tab`，它是由`ty_line`结构组成的内表。
3. **创建并填充第一个内表**：`itab1`用初始数据填充。
4. **创建并填充第二个内表**：`itab2`用不同的结构数据填充。
5. **使用**`CORRESPONDING`**操作符和**`EXCEPT`**选项复制内容**：`itab1 = VALUE #( BASE itab1 ( LINES OF CORRESPONDING #( itab2 EXCEPT comp3 ) ) )`将`itab2`的内容复制到`itab1`中，但排除`comp3`字段，并保留`itab1`中现有的内容。
6. **显示结果**：使用`cl_demo_output=>display`显示`itab1`的内容。
### 结果
运行此代码后，`itab1`将包含以下内容：

- ![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720852440189-15872c20-0ee3-4526-857f-085663f12a88.png#averageHue=%23fcfcfb&clientId=u7103dda9-8229-4&from=paste&height=256&id=u1db1d718&originHeight=256&originWidth=422&originalType=binary&ratio=1&rotation=0&showTitle=false&size=3666&status=done&style=none&taskId=uf9faaaaf-2bba-418b-aed5-4247ae5809e&title=&width=422)
### 注意事项

- **BASE**：使用`BASE`选项可以保留目标内表中的现有内容。
- **LINES OF**：使用`LINES OF`可以将另一个内表的行添加到目标内表中。
- **EXCEPT**：`EXCEPT`选项用于排除某些组件，使得在复制内容时可以选择性地排除不需要的字段。
##  使用 `CORRESPONDING` 操作符和 `DISCARDING DUPLICATES` 选项复制内容  
 为了防止在将包含重复行的数据赋给目标内表时引发运行时错误，可以使用 `CORRESPONDING` 操作符的 `DISCARDING DUPLICATES` 选项。  <br />以下示例展示了如何使用 `CORRESPONDING` 操作符和 `DISCARDING DUPLICATES` 选项在两个内表之间复制内容，同时丢弃重复的行。
### 代码
```abap
" 定义一个结构类型
TYPES: BEGIN OF ty_line,
         comp1 TYPE i,
         comp2 TYPE c LENGTH 10,
       END OF ty_line.

" 定义内表类型
TYPES ty_line_tab TYPE SORTED TABLE OF ty_line WITH UNIQUE KEY comp1.

" 创建并填充第一个内表
DATA(itab1) = VALUE ty_line_tab(
                ( comp1 = 1 comp2 = 'A' )
                ( comp1 = 2 comp2 = 'B' )
             ).

" 创建并填充第二个内表，其中包含重复的键
DATA(itab2) = VALUE ty_line_tab(
                ( comp1 = 2 comp2 = 'C' ) " 重复的键 comp1 = 2
                ( comp1 = 3 comp2 = 'D' )
             ).

" 使用 CORRESPONDING 操作符和 DISCARDING DUPLICATES 选项复制内容，同时丢弃重复的行
itab1 = CORRESPONDING #( 
          BASE itab1 
          itab2 
          DISCARDING DUPLICATES 
        ).

" 显示结果
cl_demo_output=>display( itab1 ).
```
### 
### 解释

1. **定义结构类型**：定义一个结构类型 `ty_line`，包含两个组件 `comp1` 和 `comp2`。
2. **定义内表类型**：定义一个排序的内表类型 `ty_line_tab`，并将 `comp1` 设为唯一键。
3. **创建并填充第一个内表**：使用初始数据填充第一个内表 `itab1`。
4. **创建并填充第二个内表**：第二个内表 `itab2` 包含具有重复键 `comp1 = 2` 的行。
5. **使用 **`CORRESPONDING`** 操作符和 **`DISCARDING DUPLICATES`** 选项复制内容**：通过 `CORRESPONDING` 操作符将 `itab2` 的内容复制到 `itab1` 中，同时丢弃重复的行。
6. **显示结果**：使用 `cl_demo_output=>display` 显示 `itab1` 的内容。

通过这个示例代码，我们展示了如何在两个内表之间复制内容，同时避免由于重复键导致的运行时错误。
### 结果
运行此代码后，`itab1` 将包含以下内容：

- ( comp1 = 1, comp2 = 'A' )
- ( comp1 = 2, comp2 = 'B' )
- ( comp1 = 3, comp2 = 'D' )
- ![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1720858863410-9216f459-2b1c-4ea3-bb3b-0caaabb5d699.png#averageHue=%23fdfdfd&clientId=u7103dda9-8229-4&from=paste&height=210&id=u8853e7c9&originHeight=210&originWidth=587&originalType=binary&ratio=1&rotation=0&showTitle=false&size=3007&status=done&style=none&taskId=ue48a95e5-7cdf-41cb-a18c-0e329a2ac49&title=&width=587)
### 注意事项

- **CORRESPONDING**：该操作符用于将一个内表的内容复制到另一个内表中。
- **DISCARDING DUPLICATES**：在复制过程中，丢弃重复的行，以防止因唯一键冲突而引发的运行时错误。
- **BASE**：使用 `BASE` 选项可以保留目标内表中的现有内容。
