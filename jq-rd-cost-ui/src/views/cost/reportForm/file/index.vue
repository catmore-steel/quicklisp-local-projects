<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
<!--      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:file:add']"
        >
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                   v-hasPermi="['cost:file:remove']"
        >
          删除
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="warning" icon="el-icon-download" @click="confirmExport"
                   v-hasPermi="['cost:file:export']"
        >
          导出
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="info" icon="el-icon-upload2" @click="handleImport"
                   v-hasPermi="['cost:file:import']"
        >
          导入
        </el-button>
      </el-col>-->
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
              @handleSelectionChange="handleSelectionChange"
    >
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="文件类型" prop="fileType">
                <el-select v-model="queryParams.fileType" placeholder="请选择文件类型"
                           class="form-control" clearable filterable
                >
                  <el-option
                    v-for="dict in fileTypeOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  />
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="文件名称" prop="fileName">
                <el-input
                  v-model="queryParams.fileName"
                  placeholder="请输入文件名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="文件编号" prop="fileNo">
                <el-input
                  v-model="queryParams.fileNo"
                  placeholder="请输入文件编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="operate" slot-scope="{scope}">
        <el-button type="text" @click="fileUpdate(scope.row)" v-if="scope.row.id">
          详情
        </el-button>
        <el-button type="text" icon="el-icon-collection" @click="jumIntoPage(scope.row)"
                   v-if="!scope.row.id"
        >
          进入生成
        </el-button>
        <el-button type="text" icon="el-icon-collection" @click="jumIntoPage(scope.row)" v-if="scope.row.id">
          重新生成
        </el-button>
      </template>
      <template slot="fileTypeName" slot-scope="{scope}">
        <div :style=" 'background:'+ bgColors[scope.row.dictSort]"> {{ scope.row.fileTypeName }}</div>
      </template>
    </jq-table>

    <!---汇总文件导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="汇总文件导入.xlsx"
      @handleQuery="handleQuery"
      url="/cost/file/import"
    ></import-dialog>

    <!---汇总文件导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"
    />

    <!---汇总文件查看详情 -->
    <file-detail :show="fileCard.show" v-model="fileCard.key"
                 @handleClose="fileClose"
    />

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import fileDetail from '../common/fileDetail.vue'
import { delFile, exportFile } from '@/api/cost/file'

export default {
  name: 'file',
  mixins: [JqTableMixin],
  provide() {
    return {
      handleQuery: this.handleQuery
    }
  },
  data() {
    return {
      bgColors: ['#fd96967a', '#96e0fd7a', '#96b6fd7a', '#fdca967a', '#fbfd967a', '#f296fd7a', '#a1fd967a', '#fd96a97a',
        '#96fdea7a', '#af96fd7a', '#fd96cf7a', '#fd96967a', '#96e0fd7a', '#96b6fd7a', '#fdca967a', '#fbfd967a', '#f296fd7a', '#a1fd967a', '#fd96a97a',
        '#96fdea7a', '#af96fd7a', '#fd96cf7a']
      ,
      // 选中数组
      ids: [],
      // 非单个禁用
      single: true,
      // 非多个禁用
      multiple: true,
      // 显示搜索条件
      showSearch: true,
      // 是否显示数据
      exportTitle: '导出警告',
      // 是否显示导出弹出层
      exportOpen: false,
      //导入弹出层title
      importTitle: '导入',
      // 是否显示导出弹出层
      importOpen: false,
      // 文件类型字典
      fileTypeOptions: [],
      // 文件状态字典
      fileStatusOptions: [],
      // 查询参数
      queryParams: {},
      //表格配置数据
      tableConfig: {
        url: '/cost/file/list',
        method: 'get',
        queryParams: null,
        orders: 'file.dictSort',
        exportUrl: '/cost/file/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '文件类型',
          radioSearch: true,
          radioData: [{ label: '未生成', value: 0 }, { label: '已生成', value: 1 }]
        }
      },
      fileCard: {
        show: false,
        key: null
      }
    }
  },
  components: {
    fileDetail
  },
  created() {
    this.getDicts('file_type').then(response => {
      this.fileTypeOptions = response.data
    })
    this.getDicts('file_status').then(response => {
      this.fileStatusOptions = response.data
    })
  },
  methods: {
    jumIntoPage(row) {
      let fileType = row.fileType
      let path = ''
      if (fileType == 'affirm') {
        path = ''
      } else if (fileType == 'company') {
        path = ''
      }
      this.$router.push({
        path: path
      })
    },
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },

    /** 重置按钮操作 */
    resetQuery() {
      this.resetForm('queryForm')
      this.handleQuery()
    },

    /** 多选框选中数据 */
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
    },

    /** 新增按钮操作 */
    handleAdd() {
      this.fileCard = {
        show: true,
        key: null
      }
    },

    /** 查看汇总文件 */
    fileUpdate(row) {
      this.fileCard = {
        show: true,
        key: row.id//此处id需替换为业务表唯一索引（非id）
      }
    },

    /** 关闭汇总文件查看弹框 */
    fileClose() {
      this.fileCard = {
        show: false,
        row: null
      }
    },

    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids
      this.$confirm('是否确认删除汇总文件编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delFile(ids)
      }).then(() => {
        this.getJqTableData()
        this.msgSuccess('删除成功')
      }).catch(() => {
      })
    },

    /** 打开导出页面按钮 */
    confirmExport() {
      this.queryParams.exportType = 1
      this.exportOpen = true
    },

    /** 确认导出按钮操作 */
    handleExport() {
      if (0 === this.ids.length && 3 === this.queryParams.exportType) {
        this.msgError('未选中任何数据！')
        return
      }
      this.queryParams.ids = this.ids.join(',')
      let queryParams = this.queryParams
      exportFile(queryParams)
      this.exportOpen = false
    },

    /** 打开导入页面按钮操作 */
    handleImport() {
      this.importTitle = '节假日信息导入'
      this.importOpen = true
    }
  }
}
</script>
