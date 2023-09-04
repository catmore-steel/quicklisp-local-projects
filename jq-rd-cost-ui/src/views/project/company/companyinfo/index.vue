<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['project:companyinfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                 v-hasPermi="['project:companyinfo:remove']">
          删除
        </el-button>
      </el-col>
      <el-button type="warning" icon="el-icon-download" @click="confirmExport"
                 v-hasPermi="['project:companyinfo:export']">
        导出
      </el-button>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
                  @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="成立日期" prop="foundDate">
                <el-date-picker
                  clearable size="small"
                  v-model="queryParams.foundDate"
                  class="form-control"
                  type="date"
                  value-format="yyyy-MM-dd"
                  placeholder="选择成立日期">
                </el-date-picker>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="统一信用代码" prop="creditCode">
                <el-input
                  v-model="queryParams.creditCode"
                  placeholder="请输入统一信用代码"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="企业名称" prop="companyName">
                <el-input
                  v-model="queryParams.companyName"
                  placeholder="请输入企业名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="法人代表" prop="legalName">
                <el-input
                  v-model="queryParams.legalName"
                  placeholder="请输入法人代表"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="行业类别" prop="industry">
                <treeselect v-model="queryParams.industry" :options="industries" :show-count="true" placeholder="请选择行业类别"  class="form-control"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="企业类型" prop="companyType">
                <el-select v-model="queryParams.companyType" placeholder="请选择企业类型" class="form-control" clearable filterable>
                  <el-option v-for="dict in regTypes" :key="dict.dictValue" :label="dict.dictLabel" :value="dict.dictValue" ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="行政区划" prop="divisionId">
                <el-input
                  v-model="queryParams.divisionId"
                  placeholder="请输入行政区划"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="companyName" slot-scope="{scope}">
        <a @click="companyinfoUpdate(scope.row)" style="color: #1890ff">
          {{ scope.row.companyName }}
          <i class="el-icon-document-copy" title="点击复制" @click.stop="copyNodeValue($event,scope.row.companyName)"/>
        </a>
      </template>
    </jq-table>

    <!---企业信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="企业信息导入.xlsx"
      @handleQuery="handleQuery"
      url="/project/companyinfo/import"
    ></import-dialog>

    <!---企业信息导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---企业信息查看详情 -->
    <companyinfo-detail :show="companyinfoCard.show" v-model="companyinfoCard.key" :row="row"
                    @handleClose="companyinfoClose" @handleQuery="handleQuery"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import companyinfoDetail from '../common/companyinfoDetail'
  import { delCompanyinfo,exportCompanyinfo } from '@/api/project/companyinfo'
  import { treeselectIndustry} from "@/api/conf/industry";
  import Clipboard from 'clipboard'
  import { selectItemByCompanyId } from '@/api/project/baseItemInfo'

  export default {
    name: "companyinfo",
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
          url: '/project/companyinfo/list',
          method: 'get',
          queryParams: null,
          orders: 'base_company_info.update_time desc',
          exportUrl: '/project/companyinfo/export',
          superSearch: {
            keyPlaceholder: '统一信用代码/企业名称/法人代表'
          }
        },
        companyinfoCard: {
          show: false,
          key: null
        },
        regTypes:[],
        industries:[],
        row:{}
      }
    },
    components: {
      companyinfoDetail
    },
    created() {
      this.getDicts('reg_type').then(response => { this.regTypes = response.data });
      this.apiTreeselectIndustry();
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
        this.companyinfoCard = {
          show: true,
          key: null
        }
      },

      /** 查看企业信息 */
      companyinfoUpdate(row) {
        this.companyinfoCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
        this.row=row;
      },

      /** 关闭企业信息查看弹框 */
      companyinfoClose() {
        this.companyinfoCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      async handleDelete() {
        const ids = this.ids;
        let num = 0;
        for (const i in ids) {
          await selectItemByCompanyId(ids[i]).then(res => {
            if (res.data && res.data.length > 0) {
              num++
            }
          })
        }
        if (num > 0) {
          this.msgError('该企业下存在项目，不可删除！')
        } else {
          this.$confirm('是否确认删除企业信息编号为"' + ids + '"的数据项?', "警告", {
            confirmButtonText: "确定",
            cancelButtonText: "取消",
            type: "warning"
          }).then(function () {
            return delCompanyinfo(ids);
          }).then(() => {
            this.getJqTableData()
            this.msgSuccess("删除成功");
          }).catch(() => {
          })
        }

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
        exportCompanyinfo(queryParams);
        this.exportOpen = false;
      },

      /** 打开导入页面按钮操作 */
      handleImport() {
        this.importTitle = "节假日信息导入";
        this.importOpen = true;
      },
      apiTreeselectIndustry() {
        let self = this;
        treeselectIndustry().then(resp=>{
          self.industries = self.handleTree(resp.data, "codeNumber", "parentCodeNumber");
        })
      },
    copyNodeValue(e, text) {
      const clipboard = new Clipboard(e.target, {text: () => text})
      clipboard.on('success', e => {
        this.$message.success('复制成功!')
        // 释放内存
        clipboard.off('error')
        clipboard.off('success')
        clipboard.destroy()
      })
      // err
      clipboard.on('error', e => {
        this.$message.error('复制失败!')
        clipboard.off('error')
        clipboard.off('success')
        clipboard.destroy()
      })
      clipboard.onClick(e)
    }
    }
  }
</script>
