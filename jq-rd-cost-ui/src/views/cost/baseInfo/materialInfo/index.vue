<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:materialInfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                 v-hasPermi="['cost:materialInfo:remove']">
          删除
        </el-button>
      </el-col>

    <el-col :span="1.5">
      <el-button type="warning" icon="el-icon-download" @click="confirmExport"
                 v-hasPermi="['cost:materialInfo:export']">
        导出
      </el-button>
    </el-col>
    <el-col :span="1.5">
      <el-button type="info" icon="el-icon-upload2" @click="handleImport"
                 v-hasPermi="['cost:materialInfo:import']">
        导入
      </el-button>
    </el-col>
    </el-row>


    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
                  @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6" >
              <el-form-item label="物料编号" prop="materialNo" >
                <el-input
                  v-model="queryParams.materialNo"
                  placeholder="请输入物料编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="物料名称" prop="materialName">
                <el-input
                  v-model="queryParams.materialName"
                  placeholder="请输入物料名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="规格" prop="specs">
                <el-input
                  v-model="queryParams.specs"
                  placeholder="请输入规格"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="单位" prop="unit">
                <el-input
                  v-model="queryParams.unit"
                  placeholder="请输入单位"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
            <el-form-item label="备注" prop="remark">
              <el-input
                v-model="queryParams.remark"
                placeholder="请输入备注"
                clearable
                class="form-control"
                @keyup.enter.native="handleQuery"/>
            </el-form-item>
          </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="materialNo" slot-scope="{scope}">
        <a @click="materialInfoUpdate(scope.row)" v-copy>
          {{ scope.row.materialNo }}
        </a>
      </template>
    </jq-table>

    <!---企业物料信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="企业物料信息导入模版.xlsx"
      @handleQuery="handleQuery"
      :url= "url"
    ></import-dialog>

    <!---企业物料信息导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---企业物料信息查看详情 -->
    <materialInfo-detail :show="materialInfoCard.show" v-model="materialInfoCard.key"
                    @handleClose="materialInfoClose"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import materialInfoDetail from '../common/materialInfoDetail'
  import { delMaterialInfo, exportMaterialInfo } from '@/api/cost/materialInfo'
  import { getToken } from '@/utils/auth'
  import { importTemplate } from "@/api/cost/materialInfo";

  export default {
    name: "materialInfo",
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
        // 用户导入参数
        upload: {
          // 是否显示弹出层（用户导入）
          open: false,
          // 弹出层标题（用户导入）
          title: '',
          // 是否禁用上传
          isUploading: false,
          // 是否更新已经存在的用户数据
          updateSupport: 0,
          // 设置上传的请求头部
          headers: { Authorization: 'Bearer ' + getToken() },
          // 上传的地址
          url: process.env.VUE_APP_BASE_API + '/cost/materialInfo/import'
        },
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/cost/materialInfo/list',
          method: 'get',
          queryParams: null,
          orders: 'base_material_info.update_time desc',
          exportUrl: '/cost/materialInfo/export',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '物料编号/物料名称',
            radioSearch: true,
            radioData: []
          }
        },
        materialInfoCard: {
          show: false,
          key: null
        },
      }
    },
    components: {
      materialInfoDetail
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
        this.materialInfoCard = {
          show: true,
          key: null
        }
      },

      /** 查看企业物料信息 */
      materialInfoUpdate(row) {
        this.materialInfoCard = {
          show: true,
          key: row.id,//此处id需替换为业务表唯一索引（非id）
          materialName: row.materialName
        }
      },

      /** 关闭企业物料信息查看弹框 */
      materialInfoClose() {
        this.materialInfoCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        console.log(ids)
        this.$confirm('是否确认删除企业物料信息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delMaterialInfo(ids);
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
        exportMaterialInfo(queryParams);
        this.exportOpen = false;
      },

      /** 打开导入页面按钮操作 */
      handleImport() {
        this.url = '/cost/materialInfo/import?companyId=' + this.$store.state.item.companyId + '&itemNo=' + this.$store.state.item.itemNo,
        this.importTitle = "物料信息导入";
        this.importOpen = true;
      },
    }
  }
</script>
