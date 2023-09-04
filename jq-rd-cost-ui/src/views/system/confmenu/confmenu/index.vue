<template>
  <div class="app-container conf_menu">
    <el-button type="primary" @click="changeNameOrAdd('t1',{})">新增</el-button>
    <el-tree :allow-drag="allowDrag"
             :allow-drop="allowDrop"
             :data="data"
             default-expand-all
             draggable
             node-key="id"
             style="margin-top: 20px"
             @node-drop="nodeDrop"
    >
      <div slot-scope="{node ,data}"
           class="tree_row"
      >
        <span :style="{fontSize: node.level == '1' ? '20px' : node.level == '2' ? '18px' : '16px'}">{{
            data.menuNo
          }}.{{ node.label }}</span>
        <span class="options">
          <span v-if="node.level !== 3" @click.stop="changeNameOrAdd('t2',data,node)">新增</span>
          <span @click.stop="changeNameOrAdd('change',data,node)">修改</span>
          <span @click.stop="deleteDom(data,node)">删除</span>
        </span>
      </div>
    </el-tree>
    <el-button type="success" @click="submit">提交</el-button>
  </div>
</template>

<script>
import {treeList, saveSubmit} from '@/api/system/confmenu'

export default {
  data() {
    return {
      data: [],
      defaultProps: {
        children: 'children',
        label: 'label'
      }
    }
  },
  created() {
    this.getTree()
  },
  methods: {
    // 修改名字 或者 新增
    changeNameOrAdd(type, data, node) {
      console.log(data, node)
      const typeObj = {
        t1: {
          msg: '根目录增加一级标题',
          inputValue: '',
          type: '新增'
        },
        t2: {
          msg: `为 ${data.label} 添加子标题`,
          inputValue: '',
          type: '新增'
        },
        change: {
          msg: `修改 ${data.label} 为`,
          inputValue: data.label,
          type: '修改'
        }
      }
      this.$prompt(typeObj[type].msg, typeObj[type].type, {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        inputValue: typeObj[type].inputValue,
        // inputPattern: /\s+/g,
        inputErrorMessage: '标题不能为空',
        closeOnClickModal: false
      }).then(({value}) => {
        if (type === 'change') {
          data.label = value
        } else {
          // 分为根目录添加一级目录 和 一级目录下的二/三级目录
          if (type == 't1') {
            this.data.push({label: value, children: []})
          } else {
            console.log('data.children',data.children)
            if (!data.children) {
              this.$set(data,'children',[])
            }
            data.children.push({label: value})
          }
          this.setMenuNo()
        }
      }).catch(() => {
        this.$message({
          type: 'info',
          message: '取消修改'
        })
      })
    },
    // 删除
    deleteDom(data, node) {
      this.$confirm('此操作将删除改目录已经其目录下的文件, 是否继续?', '提示', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(() => {
        if (node.parent.data.children) {
          node.parent.data.children.splice(data.menuNo - 1, 1)
        } else {
          console.log(data)
          const index = this.data.findIndex(item => item.menuNo == data.menuNo)
          node.parent.data.splice(index, 1)
        }
        this.setMenuNo()
      }).catch(() => {
        this.$message({
          type: 'info',
          message: '已取消删除'
        });
      });
    },
    // 节点拖拽结束
    nodeDrop(draggingNode, dropNode, dropType, ev) {
      this.setMenuNo()
    },

    // 拖拽 / 新增 后重新排序
    setMenuNo() {
      this.data.forEach((item, index) => {
        item.menuNo = this.convertToChinaNum(index + 1)
        item.children && item.children.forEach((item2, index2) => {
          item2.menuNo = index2 + 1
          item2.children && item2.children.forEach((item3, index3) => {
            item3.menuNo = index3 + 1
          })
        })
      })
    },

    // 提交
    submit() {
      console.log(this.data)
      saveSubmit(this.data).then(response => {
        if (response.code === 200) {
          this.msgSuccess(response.msg)
          this.handleClose()
          this.handleQuery()
        }
      });
    },
    // 数字转汉字
    convertToChinaNum(num) {
      let arr1 = ['零', '一', '二', '三', '四', '五', '六', '七', '八', '九']
      let arr2 = ['', '十', '百', '千', '万', '十', '百']
      if (!num || isNaN(num)) {
        return '零'
      }
      let english = num.toString().split('')
      let result = ''
      for (let i = 0; i < english.length; i++) {
        let des_i = english.length - 1 - i//倒序排列设值
        result = arr2[i] + result
        let arr1_index = english[des_i]
        result = arr1[arr1_index] + result
      }
      //将【零千、零百】换成【零】 【十零】换成【十】
      result = result.replace(/零(千|百|十)/g, '零').replace(/十零/g, '十')
      //合并中间多个零为一个零
      result = result.replace(/零+/g, '零')
      //将【零亿】换成【亿】【零万】换成【万】
      result = result.replace(/零亿/g, '亿').replace(/零万/g, '万')
      //将【亿万】换成【亿】
      result = result.replace(/亿万/g, '亿')
      //移除末尾的零
      result = result.replace(/零+$/, '')
      //将【零一十】换成【零十】
      //result = result.replace(/零一十/g, '零十');//貌似正规读法是零一十
      //将【一十】换成【十】
      result = result.replace(/^一十/g, '十')
      return result
    },
    // 是否可以放置在当前位置
    allowDrop(draggingNode, dropNode, type) {
      if (draggingNode.level === dropNode.level && type !== 'inner') {
        return true
      }
      if (draggingNode.level > dropNode.level && type == 'inner') {
        return true
      }

    },
    // 节点是否可以被拖拽
    allowDrag(draggingNode) {
      return true
    },
    async getTree() {
      const data = await treeList()
      if (data && data.code == 200) {
        this.data = data.data
      }
    }
  }
}
</script>

<style lang="scss">
.conf_menu {
  .el-tree-node__content {
    height: 36px;
  }

  .tree_row {
    width: 40%;

    .options {
      float: right;
      display: none;

      span {
        margin-left: 10px;
      }
    }

    &:hover {
      .options {
        display: inline-block;
      }
    }
  }
}

</style>
