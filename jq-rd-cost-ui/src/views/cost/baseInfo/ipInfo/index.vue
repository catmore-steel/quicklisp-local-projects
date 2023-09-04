<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:ipInfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                   v-hasPermi="['cost:ipInfo:remove']">
          删除
        </el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
              @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="资产编号" prop="ipNo">
                <el-input
                  v-model="queryParams.ipNo"
                  placeholder="请输入资产编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="资产名称" prop="ipName">
                <el-input
                  v-model="queryParams.ipName"
                  placeholder="请输入资产名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="使用部门" class="demonstration">
                <treeselect v-model="queryParams.getDeptId" :multiple="false" :options="getDeptIdOptions"  class="form-control"
                            placeholder="请选择使用部门"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="登记编号" prop="registeNo">
                <el-input
                  v-model="queryParams.registeNo"
                  placeholder="请输入登记编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="资产类型" prop="ipType">
                <el-select v-model="queryParams.ipType" placeholder="请选择资产类型" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in ipTypeOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="ipNo" slot-scope="{scope}">
        <a @click="ipInfoUpdate(scope.row)" v-copy>
          {{ scope.row.ipNo }}
        </a>
      </template>
    </jq-table>

    <!---无形资产信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="无形资产信息导入.xlsx"
      @handleQuery="handleQuery"
      url="/cost/ipInfo/import"
    ></import-dialog>

    <!---无形资产信息导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---无形资产信息查看详情 -->
    <ipInfo-detail :show="ipInfoCard.show" v-model="ipInfoCard.key"
                   @handleClose="ipInfoClose"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import ipInfoDetail from '../common/ipInfoDetail'
  import { delIpInfo } from '@/api/cost/ipInfo'
  import { getDeptNameInfo } from '@/api/cost/deptInfo'

  export default {
    name: 'ipInfo',
    mixins: [JqTableMixin],
    provide() {
      return {
        handleQuery: this.handleQuery
      }
    },
    data() {
      return {
        getDeptIdOptions: [],
        // 设备类型字典
        ipTypeOptions: [],
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
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/cost/ipInfo/list',
          method: 'get',
          queryParams: null,
          orders: 'base_ip_info.update_time desc',
          exportUrl: '/cost/ipInfo/export',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '',
            radioSearch: true,
            radioData: []
          }
        },
        ipInfoCard: {
          show: false,
          key: null
        }
      }
    },
    components: {
      ipInfoDetail
    },
    created() {
      //获取全局的的监听下拉框部门数据
      this.mounted()

      //资产类型字典
      this.getDicts('ip_type').then(response => {
        this.ipTypeOptions = response.data
      })
    },
    methods: {
      mounted(){
        this.$bus.$on('response',(row)=> {
          this.getDeptIdOptions = row
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
        this.ipInfoCard = {
          show: true,
          key: null
        }
      },

      /** 查看无形资产信息 */
      ipInfoUpdate(row) {
        this.ipInfoCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭无形资产信息查看弹框 */
      ipInfoClose() {
        this.ipInfoCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids
        this.$confirm('是否确认删除无形资产信息编号为"' + ids + '"的数据项?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          return delIpInfo(ids)
        }).then(() => {
          this.getJqTableData()
          this.msgSuccess('删除成功')
          this.handleClose()
          this.handleQuery()
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
        exportIpInfo(queryParams)
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
