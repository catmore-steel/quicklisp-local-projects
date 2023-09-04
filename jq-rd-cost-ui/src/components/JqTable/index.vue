<template>
  <div class="jq_table" :key="key">
    <!-- 表格顶部的工具 -->
    <div class="mb8 opt_row">
      <div v-if="config.superSearch" class="super_search_slot">
        <el-radio-group v-model="radioSearch" v-if="config.superSearch.radioSearch" @change="getList">
          <el-radio-button :label="null">全部</el-radio-button>
          <el-radio-button v-for="item in config.superSearch.radioData" :label="item.value">{{ item.label }}
          </el-radio-button>
        </el-radio-group>
        <el-tooltip class="item" effect="dark" :content="config.superSearch.keyPlaceholder" placement="top">
          <el-input v-model="keySearch" placeholder="请输入关键字"
                    clearable @clear="getList" @keyup.enter.native="getList">
            <i slot="prefix" class="el-input__icon el-icon-search"></i>
          </el-input>
        </el-tooltip>
      </div>
      <div v-if="$scopedSlots.search" style="margin-right: 7px;">
        <el-button type="primary" @click="handleShowSearch">
          <i :class="[showSearch?'is-open':'','el-icon-arrow-down','search-down']"></i> 更多筛选
        </el-button>
      </div>
      <div>
        <el-tooltip class="item" effect="dark" content="重置" placement="top" style="margin-right: 7px;">
          <el-button circle icon="el-icon-refresh" @click="resetQuery"/>
        </el-tooltip>
        <el-tooltip class="item" effect="dark" content="设置" placement="top">
          <el-dropdown trigger="click" ref="drop" @visible-change="handDropDown" :hide-on-click="false">
            <el-button circle icon="el-icon-s-operation"/>
            <el-dropdown-menu slot="dropdown">
              <!-- 全选 -->
              <el-dropdown-item class="check_all">
                <span>全选</span>
                <el-checkbox :indeterminate="isIndeterminate" @change="handleCheckAllChange"></el-checkbox>
              </el-dropdown-item>
              <div v-if="fixedList.length > 0" class="div1">
                <el-dropdown-item v-for="item in fixedList" :key="item.columnName"
                                  style="position: relative;width:97%;overflow:hidden;white-space: nowrap;text-overflow: ellipsis;">
                  <i class="el-icon-lock"></i> {{ item.columnLabel }}
                  <el-checkbox v-model="item.isShow"></el-checkbox>
                </el-dropdown-item>
              </div>
              <div class="div2">
                <el-dropdown-item  v-for="item in normalList" :key="item.columnName"
                                   style="position: relative;width:97%;overflow:hidden;white-space: nowrap;text-overflow: ellipsis;">
                  {{ item.columnLabel }}
                  <el-checkbox v-model="item.isShow"></el-checkbox>
                </el-dropdown-item>
              </div>
              <el-dropdown-item class="control">
                <el-button type="primary" @click="submitForm">确 认
                </el-button>
                <el-button type="info" @click="cancel">取 消
                </el-button>
              </el-dropdown-item>
            </el-dropdown-menu>
          </el-dropdown>
        </el-tooltip>
      </div>
    </div>
    <el-collapse-transition>
      <div v-show="showSearch" :class="['search_box_content',lockStatus?'lock':'unlock']">
        <div style="padding: 15px 10px 10px 10px;">
          <slot name="search"></slot>
        </div>
        <div class="search_box_operate">
          <el-button type="primary" icon="el-icon-search" @click="handleQuery">搜索</el-button>
          <el-button icon="el-icon-refresh" @click="resetQuery">重置</el-button>
          <el-button type="success" icon="el-icon-lock" @click="lock">{{ lockStatus ? '解锁' : '锁定' }}
          </el-button>
        </div>
      </div>
    </el-collapse-transition>
    <!-- 表格 -->
    <el-table :data="tableData"
              max-height="680"
              :key="tableKey"
              @selection-change="handleSelectionChange"
              @row-click="rowClick"
              @sort-change="sortChange"
              :row-class-name="tableRowClassName"
              :row-key="getRowKey">
      <el-table-column v-if="isSelection" type="selection" width="55" :selectable="selectable" :reserve-selection="isKeepRow"
                       align="center"/>
      <el-table-column label="操作" :sortable="false" min-width="300" fixed v-if="$scopedSlots.operate">
        <template slot-scope="scope">
          <slot name="operate" :scope="scope"></slot>
        </template>
      </el-table-column>
      <template v-for="item in tableHeadList">
        <el-table-column :prop="item.columnName"
                         show-overflow-tooltip
                         :key="item.columnName"
                         :width="item.columnWidth"
                         v-if="item.isShow && !item.hidden"
                         :sort-by="item.orderBy"
                         :fixed="!!item.isLock">
          <template slot="header">
            <span> {{ item.columnLabel }} <i @click.stop="handleLock(item)"
                                             :class="[item.isLock ? 'el-icon-lock' :'el-icon-unlock']"></i></span>
          </template>
          <template slot-scope="scope">
            <slot :name="item.columnName"
                  :scope="scope">{{ scope.row[item.columnName] }}
            </slot>
          </template>
        </el-table-column>
      </template>
    </el-table>

    <div class="pagination_wrap">
      <!--  删除选中  -->
      <span class="statistics" v-show="(checkLength > 0) && isExport">已选中
      <span>{{ checkLength }}</span>条数据
         <el-button
           type="warning"
           icon="el-icon-download"

           @click="handleExport($event)">导出选中
        </el-button>
      </span>
      <!--  分页  -->
      <pagination
        v-show="total>0"
        :total="total"
        :limit.sync="pageSize"
        @pagination="handPagination"
      />
    </div>
  </div>

</template>

<script>
import Sortable from 'sortablejs';

import {lockColumn, showColumn, getTableDate, exportCheckRow} from '@/api/system/tableConfig'
import { getDeptNameInfo } from '@/api/cost/deptInfo'

export default {
  name: 'JqTable',
  data() {
    return {
      key: 0,
      tableKey: null,
      showSearch: false,  // 显示搜索
      lockStatus: false,
      tableData: [],  // 表格的数据
      tableHeadList: [],   // 表头的数据
      newTableHeadList: [], // 用于 用户自定义拖拽表头
      fixedList : [], // 固定的表头
      normalList: [], // 普通表头（非固定）
      backValue: [],  // 用户取消自定义表头 重置 表头数据
      isIndeterminate: false,
      checkAll: false,
      checkRows: [],  // 表格选中的 row
      checkLength: 0,  // 表格选中的 row 的条数
      ids: [], // 表格选中的id arr
      pageNum: 1,
      pageSize: 15,
      orders: null,
      total: 0,
      radioSearch: null,
      keySearch: null,
      globalFlg: false  //顶部全局 查询标签
    }
  },
  props: {
    tableDataObj: Object,
    // 分页后是否保留选中数据
    isKeepRow: {
      type: Boolean,
      default: false
    },
    config: {
      type: Object,
      require: true
    },
    queryParams: {
      type: Object,
      require: true
    },
    // 表格行的拖拽排序
    drag: {
      type: Boolean,
      default: false
    },
    isSelection: {
      type: Boolean,
      default: true
    },
    isExport:{
      type: Boolean,
      default: true
    }
  },
  watch:{
    '$store.state.item.itemNo'(newVal,oldVal){
      this.$nextTick(()=>{
        if (this.globalFlg){
          this.queryParams.itemNo = this.$store.state.item.itemNo;
          this.queryParams.companyId = this.$store.state.item.companyId;
          this.getDeptNameInfo1(this.queryParams.companyId,this.queryParams.itemNo)
        }
        this.getList()
      })
    }
  },
  created() {
    this.globalFlg = this.config.globalFlg
    this.orders = this.config.orders
    this.pageSize = this.config.pageSize? this.config.pageSize:15
    this.$nextTick(()=>{
      if (this.globalFlg){
        this.queryParams.itemNo = this.$store.state.item.itemNo;
        this.queryParams.companyId = this.$store.state.item.companyId;
      }
      this.getList()
    })
  },
  mounted() {
    if(this.drag){
      this.rowDrop();
    }
  },
  methods: {
    // 展示/隐藏 设置的下拉
    handDropDown(val) {
      if (val) {
        this.newTableHeadList = JSON.parse(JSON.stringify(this.tableHeadList))
        this.fixedList = this.newTableHeadList.filter(item => item.isLock && !item.hidden)  // 固定的列
        this.normalList = this.newTableHeadList.filter(item => !item.isLock && !item.hidden)  // 普通的列（非固定）
        this.backValue = JSON.parse(JSON.stringify(this.tableHeadList))
      }
      this.tableHeadDrop();
    },
    tableHeadDrop(){
      this.$nextTick(()=>{
        const div1Dom = document.querySelector('.div1')
        const div2Dom = document.querySelector('.div2')
        let _this = this
        div1Dom && Sortable.create(div1Dom,{
          animation: 150,
          onEnd({newIndex,oldIndex}){
            _this.fixedList.splice(newIndex,0,_this.fixedList.splice(oldIndex,1)[0])
          }
        })
        div2Dom && Sortable.create(div2Dom,{
          animation: 150,
          onEnd({newIndex,oldIndex}){
            _this.normalList.splice(newIndex,0,_this.normalList.splice(oldIndex,1)[0])
          }
        })
      })
    },
    // 获取表格数据
    async getList() {
      const {url, method} = this.config
      let queryParams = this.queryParams;
      const {pageNum, pageSize, orders, radioSearch, keySearch} = this
      const {menuKey} = this.$route.query
      let query = {...queryParams, pageNum, pageSize, orders, radioSearch, keySearch, menuKey};
      const response = await getTableDate(url, method, query)
      if (response && response.code === 200) {
        this.total = response.data.total
        this.tableData = response.data.tableBodyList
        this.tableHeadList = response.data.tableHeadList
      } else {
        this.$message.error('加载表格失败！')
      }
    },
    handleQuery() {
      this.getList();
      this.closeSearch();
    },
    /** 重置按钮操作 */
    resetQuery() {
      this.radioSearch = null
      this.keySearch = null
      //清空 Search表单
      if (this.globalFlg) {
        this.$emit('update:queryParams', {
          itemNo: this.$store.state.item.itemNo,
          companyId: this.$store.state.item.companyId
        })
      } else {
        this.$emit('update:queryParams', {})
      }
      this.$emit('resetQuery')
      this.$nextTick(() => {
        this.getList();
        this.closeSearch();
      })
    },

    getRowKey(row) {
      return row.id
    },
    // table row 的 选择
    handleSelectionChange(val) {
      this.checkLength = val.length
      this.checkRows = val
      this.ids = val.map(item => item.id)
      this.$emit('handleSelectionChange', val)
    },
    selectable(row, index) {
      let result = true
      this.$emit("selectable", row, index, val => {
        result = val
      });
      return result
    },
    rowClick(row, column, event) {
      this.closeSearch();
      this.$emit('rowClick', row, column, event)
    },
    // 多选的表格行高亮
    tableRowClassName({row, rowIndex}) {
      var arr = this.ids;
      for (let i = 0; i < arr.length; i++) {
        if (row.id == arr[i]) {
          return 'row_active'
        }
      }
    },
    // 表格的行排序
    sortChange({column, order}) {
      let str = order == "descending" ? 'desc' : order == "ascending" ? 'asc' : 'asc'
      this.orders = column.sortBy + ' ' + str
      this.getList()
    },
    // 点击表头的 锁
    async handleLock(item) {
      const {menuKey} = this.$route.query
      let postVal = {
        menuKey,
        columnName: item.columnName,
        isLock: item.isLock === 1 ? 0 : 1
      }
      const data = await lockColumn(postVal)
      if (data && data.code === 200) {
        this.getList()
        this.$message.success('修改成功!')
        setTimeout(() => {
          this.tableKey = Math.random()
          if(this.drag){
            this.rowDrop();
          }
        }, 500)
      }
    },
    // 修改 循序 与 显示 的确定
    async submitForm() {
      // 给每一个 item 加一个序号
      let tableHeadEntityList = []
      const {menuKey} = this.$route.query
      let hiddenList = this.tableHeadList.filter(item => item.hidden) // 把被关闭显示的tableheader 提取拼接回去
      let headerListAll = this.fixedList.concat(this.normalList).concat(hiddenList) // 固定列 普通列 不显示列 的数组
      headerListAll.forEach((item, index) => {
        tableHeadEntityList.push({
          columnName: item.columnName,
          isShow: item.isShow,
          columnOrder: index
        })
      })
      let postVal = {
        menuKey,
        tableHeadEntityList
      }
      const data = await showColumn(postVal)
      if (data && data.code === 200) {
        this.getList()
        this.$refs.drop.hide()
        this.$message.success('修改成功!')
        setTimeout(() => {
          this.tableKey = Math.random()
        }, 500)
      } else {
        this.$message.error('修改失败!')
      }
    },
    // 修改 循序 与 显示 的取消
    cancel() {
      this.$refs.drop.hide()
      this.newTableHeadList = JSON.parse(JSON.stringify(this.backValue))
    },
    // 循序 与 显示 的 全选
    handleCheckAllChange(val) {
      this.newTableHeadList.map(ele => ele.isShow = val)
      this.isIndeterminate = false
    },
    // 导出选中数据
    handleExport(e) {
      const {exportUrl, method} = this.config
      const idsStr = this.checkRows.map(item => item.id).join(",");
      exportCheckRow(exportUrl, {exportType: 3, ids: idsStr}, e)
    },
    // 点击分页
    handPagination(val) {
      this.pageNum = val.page
      this.pageSize = val.limit
      this.getList(val)
    },
    handleShowSearch() {
      if (this.lockStatus) {
        this.msgError("请先解锁搜索栏！")
        return
      }
      this.showSearch = !this.showSearch
      if (this.showSearch == true) {
        //收起操作 同时解锁
        this.lockStatus = false
      }
    },
    lock() {
      this.lockStatus = !this.lockStatus
    },
    closeSearch() {
      if (this.lockStatus == false && this.showSearch == true) {
        this.showSearch = false
      }
    },
    initRowDrop(dom){
      const _this = this;
      Sortable.create(dom, {
        animation: 150,
        onEnd({ newIndex, oldIndex }) {
          if(newIndex == oldIndex ) return  // 位置不动 return
          const currRow = _this.tableData.splice(oldIndex, 1)[0];
          _this.tableData.splice(newIndex, 0, currRow);
          _this.$confirm('是否保存排序?', '提示', {
            confirmButtonText: '确定',
            cancelButtonText: '取消',
            type: 'warning'
          }).then(() => {
            _this.$emit('rowDrop',newIndex, oldIndex,_this.tableData[newIndex],_this.pageNum,_this.pageSize) // 新的index,旧的index,row,page,size
          }).catch(() => {
            _this.$message({
              type: 'info',
              message: '已取消保存'
            });
            _this.getList()
          });
        }
      });
    },
    // 行的拖拽
    rowDrop() {
      setTimeout(()=>{
        const tbody = document.querySelector(".jq_table .el-table__body-wrapper tbody");
        const fixedTable = document.querySelector(".jq_table .el-table__fixed-body-wrapper tbody");// fixed 的列
        tbody && this.initRowDrop(tbody)
        fixedTable && this.initRowDrop(fixedTable)
      },1000)
    },
    /** 查询部门下拉树结构 */
    getDeptNameInfo1(companyId,itemNo) {
      getDeptNameInfo(companyId,itemNo).then(response => {
        this.send(response.data);
      })
    },
    send(row){
      this.$bus.$emit('response',row)
    }
  }
}
</script>

<style lang="scss" scoped>
.jq_table {
  position: relative;

  /deep/ .el-table {

    .el-icon-document-copy {
      cursor: pointer;
      font-weight: 600;
      font-size: 16px;
      margin-left: 5px;
      color: #ccc;
    }

    .el-icon-document-copy:hover {
      color: #409EFF;
    }


    .row_active, .row_active .el-table__cell {
      background-color: #B0E0E6 !important;
    }

  }


}

.opt_row {
  display: flex;
  justify-content: flex-end;


  /deep/ .el-tooltip {
    font-size: 12px;
  }

  .super_search_slot {

    /deep/ .el-input {
      width: 250px;
    }


    div {
      margin-right: 7px;
    }
  }
}

.el-dropdown-menu {
  //position: relative;
  max-height: 60%;
  overflow-y: auto;
  overflow-x: hidden;
}

.el-checkbox {
  position: absolute;
  right: 0px;
}

.el-dropdown-menu__item {
  cursor: move;
}

.check_all {
  cursor: pointer;
  font-weight: 600;
  border-bottom: 2px solid #eee;
  position: relative;
  width: 97%;
}

.check_all:hover, .control:focus {
  background-color: #fff;
  color: #000;
}

  .div1{
    i{
      margin-right: 0px;
      font-weight: 600;
    }
  }
  .div1 li:last-child{
    border-bottom: 2px solid #eee;
  }

.control {
  position: sticky;
  bottom: 0px;
  cursor: pointer;
  text-align: center;
}

.control:hover, .control:focus {
  background-color: #fff;
}

.cell {
  position: relative;

  i {
    position: absolute;
    top: 50%;
    margin-top: -9px;
    right: 20px;
    font-size: 18px;
    font-weight: 600;
    color: #ccc;
  }

  .el-icon-unlock:hover {
    color: #00ada7;
  }

  .el-icon-lock:hover {
    color: #00ada7;
  }


}

.cell:hover {
  .sort-caret {
    color: #00ada7;
  }
}


.pagination_wrap {
  position: relative;

  .statistics {
    z-index: 2;
    margin-top: 15px;
    position: absolute;
    font-size: 15px;
    font-weight: 400;

    span {
      display: inline-block;
      padding: 0 5px;
    }
  }
}

.search_box_operate {
  background: #eef1f6;
  padding: 5px;
  text-align: center;
  align-content: center;

  /deep/ .el-button {
    margin-left: 10px !important;
  }
}

.search_box_content {
  z-index: 9;
  background: white;
  border: 1px solid #dfe6ec;
}


.unlock {
  position: absolute;
  left: 0;
  right: 0;
  z-index: 9;
}

.lock {
  width: calc(100%);
  position: relative;
  margin-top: 0px;
}

.search-down {
  -webkit-transition: -webkit-transform .3s;
  transition: -webkit-transform .3s;
  transition: transform .3s;
  transition: transform .3s, -webkit-transform .3s;
}

.is-open {
  -webkit-transform: rotateZ(180deg);
  transform: rotateZ(180deg);
}

</style>
