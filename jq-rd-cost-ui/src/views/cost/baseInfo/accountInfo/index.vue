<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:accountInfo:add']">
          新增
        </el-button>
      </el-col>

      <el-col :span="1.5">
        <el-button type="info" icon="el-icon-upload el-icon--right" @click="handleImport"
                   v-hasPermi="['cost:accountInfo:import']" >
          序时账导入
        </el-button>
      </el-col>

      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                 v-hasPermi="['cost:accountInfo:remove']">
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
              <el-form-item label="日期" prop="accDate">
                <el-date-picker
                  clearable size="small"
                  v-model="queryParams.accDate"
                  class="form-control"
                  type="date"
                  value-format="yyyy-MM-dd"
                  placeholder="选择日期">
                </el-date-picker>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="凭证号" prop="credentialsNo">
                <el-input
                  v-model="queryParams.credentialsNo"
                  placeholder="请输入凭证号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="摘要" prop="summary">
                <el-input
                  v-model="queryParams.summary"
                  placeholder="请输入摘要"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="会计科目" prop="accountSubject">
                <el-input
                  v-model="queryParams.accountSubject"
                  placeholder="请输入会计科目"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="科目编码" prop="subjectCode">
                <el-input
                  v-model="queryParams.subjectCode"
                  placeholder="请输入科目编码"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="accountNo" slot-scope="{scope}">
        <a @click="accountInfoUpdate(scope.row)" v-copy>
          {{ scope.row.accountNo }}
        </a>
      </template>
    </jq-table>

    <!---原始序时账信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="原始序时账信息导入模版.xlsx"
      @handleQuery="handleQuery"
      :url= "url"
    ></import-dialog>

    <!---原始序时账信息导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---原始序时账信息查看详情 -->
    <accountInfo-detail :show="accountInfoCard.show" v-model="accountInfoCard.key"
                    @handleClose="accountInfoClose"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import accountInfoDetail from '../common/accountInfoDetail'
  import { delAccountInfo, exportAccountInfo } from '@/api/cost/accountInfo'

  export default {
    name: "accountInfo",
    mixins: [JqTableMixin],
    provide() {
      return {
        handleQuery: this.handleQuery
      }
    },
    data() {
      return {
        // 选中数组
        ids: [],
        // 非单个禁用
        single: true,
        // 非多个禁用
        multiple: true,
        // 显示搜索条件
        showSearch: true,
        // 是否显示数据
        exportTitle: "导出警告",
        // 是否显示导出弹出层
        exportOpen: false,
        //导入弹出层title
        importTitle: "导入",
        // 是否显示导出弹出层
        importOpen: false,
        url: "",
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/cost/accountInfo/list',
          method: 'get',
          queryParams: null,
          orders: 'base_account_info.account_no',
          exportUrl: '/cost/accountInfo/export',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '摘要',
            radioSearch: true,
            radioData: []
          }
        },
        accountInfoCard: {
          show: false,
          key: null
        },
      }
    },
    components: {
      accountInfoDetail
    },
    created() {
    },
    methods: {
      /** 搜索按钮操作 */
      handleQuery() {
        this.tableConfig.queryParams = this.queryParams
        this.getJqTableData()
      },

      /** 重置按钮操作 */
      resetQuery() {
        this.resetForm("queryForm");
        this.handleQuery();
      },

      /** 多选框选中数据 */
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
      },

      /** 新增按钮操作 */
      handleAdd() {
        this.accountInfoCard = {
          show: true,
          key: null
        }
      },

      /** 查看原始序时账信息 */
      accountInfoUpdate(row) {
        this.accountInfoCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭原始序时账信息查看弹框 */
      accountInfoClose() {
        this.accountInfoCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除原始序时账信息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delAccountInfo(ids);
        }).then(() => {
          this.getJqTableData()
          this.msgSuccess("删除成功");
        }).catch(() => {
        })
      },

      /** 打开导出页面按钮 */
      confirmExport() {
        this.queryParams.exportType = 1;
        this.exportOpen = true;
      },

      /** 确认导出按钮操作 */
      handleExport() {
        if (0 === this.ids.length && 3 === this.queryParams.exportType) {
          this.msgError("未选中任何数据！");
          return
        }
        this.queryParams.ids = this.ids.join(",");
        let queryParams = { ...this.queryParams, ...this.getPageSize() }
        queryParams.menuKey = this.$route.query.menuKey
        exportAccountInfo(queryParams);
        this.exportOpen = false;
      },

      /** 打开导入页面按钮操作 */
      handleImport() {
        this.url = '/cost/accountInfo/import?companyId=' + this.$store.state.item.companyId + '&itemNo=' + this.$store.state.item.itemNo,
        this.importTitle = "原始序时账信息导入";
        this.importOpen = true;
      },
    }
  }
</script>
