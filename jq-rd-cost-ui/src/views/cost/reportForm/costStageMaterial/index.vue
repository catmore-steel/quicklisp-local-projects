<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-select v-model="queryParams.projectId" placeholder="请选择研发项目" @change="handleQuery">
          <el-option
            v-for="item in allProjectList"
            :key="item.id"
            :label="`${item.projectNo} - ${item.projectName}`"
            :value="item.id"
          >
          </el-option>
        </el-select>
      </el-col>
      <el-col :span="1.5">
      <el-button type="primary" @click="confirmExport" v-hasPermi="['cost:costStageMaterial:export']" >
        生成燃料动力分摊表
      </el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :isSelection="false" > </jq-table>


    <!---研发阶段直接投入明细导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>



  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import { getAllProject } from '@/api/cost/projectInfo'
  import { delStageMaterial,exportStageMaterial  } from '@/api/cost/costStageMaterial'

  export default {
    name: "stageMaterial",
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
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/cost/costStageMaterial/listFlat',
          method: 'get',
          queryParams: null,
          orders: 'cost_stage_material.update_time desc',
          exportUrl: '/cost/costStageMaterial/export',
          globalFlg: true
        },
        stageMaterialCard: {
          show: false,
          key: null
        },
        allProjectList:[],
        itemNo: this.$store.state.item.itemNo,
        companyId: this.$store.state.item.companyId,
      }
    },
    components: {
      
    },
    created() {
      this.findProjectInfo()
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
        this.stageMaterialCard = {
          show: true,
          key: null
        }
      },

      /** 查看研发阶段直接投入明细 */
      stageMaterialUpdate(row) {
        this.stageMaterialCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭研发阶段直接投入明细查看弹框 */
      stageMaterialClose() {
        this.stageMaterialCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除研发阶段直接投入明细编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delStageMaterial(ids);
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
        exportStageMaterial(queryParams);
        this.exportOpen = false;
      },
      findProjectInfo(){
        getAllProject({itemNo:this.itemNo,companyId:this.companyId}).then(res => {
          this.allProjectList = res.data
        })
      }
    },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.handleQuery()
        this.findProjectInfo()
      })
    }
  }
  }
</script>
<style lang="scss" scoped>

::v-deep .el-icon-unlock {
      display: none;
}

</style>