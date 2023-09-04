<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['conf:confAccountConfig:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                 v-hasPermi="['conf:confAccountConfig:remove']">
          删除
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="warning" icon="el-icon-upload2" @click="handleImport"
                   v-hasPermi="['conf:confAccountConfig:import']">
          导入
        </el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
                  @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="科目编码" prop="code">
                <el-input
                  v-model="queryParams.code"
                  placeholder="请输入科目编码"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="会计科目" prop="name">
                <el-input
                  v-model="queryParams.name"
                  placeholder="请输入会计科目"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="operate" slot-scope="{scope}">
        <el-button type="text" @click="confAccountConfigUpdate(scope.row)">
          编辑
        </el-button>
      </template>
      <template slot="id" slot-scope="{scope}">
        <a @click="confAccountConfigUpdate(scope.row)" v-copy>
          {{ scope.row.id }}
        </a>
      </template>
    </jq-table>

    <!---会计科目配置导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="会计科目配置导入.xlsx"
      @handleQuery="handleQuery"
      url="/conf/confAccountConfig/import"
    ></import-dialog>

    <!---会计科目配置导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---会计科目配置查看详情 -->
    <confAccountConfig-detail :show="confAccountConfigCard.show" v-model="confAccountConfigCard.key"
                    @handleClose="confAccountConfigClose"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import confAccountConfigDetail from '../common/confAccountConfigDetail'
  import { delConfAccountConfig } from '@/api/conf/confAccountConfig'

  export default {
    name: "confAccountConfig",
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
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/conf/confAccountConfig/list',
          method: 'get',
          queryParams: null,
          orders: 'conf_account_config.update_time desc',
          exportUrl: '/conf/confAccountConfig/export',
          superSearch: {
            keyPlaceholder: '',
            radioSearch: false,
            radioData: [{label: '搜索一', value: 1}, {label: '搜索二', value: 2}]
          }
        },
        confAccountConfigCard: {
          show: false,
          key: null
        },
      }
    },
    components: {
      confAccountConfigDetail
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
        this.confAccountConfigCard = {
          show: true,
          key: null
        }
      },

      /** 查看会计科目配置 */
      confAccountConfigUpdate(row) {
        this.confAccountConfigCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭会计科目配置查看弹框 */
      confAccountConfigClose() {
        this.confAccountConfigCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除会计科目配置编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delConfAccountConfig(ids);
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
        let queryParams = this.queryParams;
        exportConfAccountConfig(queryParams);
        this.exportOpen = false;
      },

      /** 打开导入页面按钮操作 */
      handleImport() {
        this.importTitle = "节假日信息导入";
        this.importOpen = true;
      },
    }
  }
</script>
