<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :span="4" :xs="24" style="border-right: 2px solid #bbd1ad;    height: calc(100vh - 127px);overflow: auto;">
        <div class="head-container">
          <el-input
            v-model="deptName"
            placeholder="请输入部门名称"
            clearable
            prefix-icon="el-icon-search"
            style="margin-bottom: 20px"
          />
        </div>
        <div class="head-container">
          <el-tree
            :data="deptInfos"
            :props="defaultProps"
            :expand-on-click-node="false"
            :filter-node-method="filterNode"
            :default-expanded-keys="[1]"
            node-key="id"
            ref="tree"
            @node-click="handleNodeClick"
          />
        </div>
      </el-col>
      <!--用户数据-->
      <el-col :span="20" :xs="24">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:deptInfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                 v-hasPermi="['cost:deptInfo:remove']">
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
              <el-form-item label="部门名称" prop="deptName">
                <el-input
                  v-model="queryParams.deptName"
                  placeholder="请输入部门名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="设备类型" prop="deptType">
                <el-select v-model="queryParams.deptType" placeholder="请选择部门类型" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in deptTypeOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="工资" prop="payWages">
                <el-input
                  v-model="queryParams.payWages"
                  placeholder="请输入工资"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="奖金" prop="payBonus">
                <el-input
                  v-model="queryParams.payBonus"
                  placeholder="请输入奖金"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="其他工资" prop="payOtherWages">
                <el-input
                  v-model="queryParams.payOtherWages"
                  placeholder="请输入其他工资"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="保险" prop="payInsure">
                <el-input
                  v-model="queryParams.payInsure"
                  placeholder="请输入保险"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="公积金" prop="payAccfund">
                <el-input
                  v-model="queryParams.payAccfund"
                  placeholder="请输入公积金"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="其他保险" prop="payOtherInsure">
                <el-input
                  v-model="queryParams.payOtherInsure"
                  placeholder="请输入其他保险"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="折旧" prop="payDepreciation">
                <el-input
                  v-model="queryParams.payDepreciation"
                  placeholder="请输入折旧"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="无形资产" prop="payAssets">
                <el-input
                  v-model="queryParams.payAssets"
                  placeholder="请输入无形资产"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="材料科目" prop="paySubject">
                <el-input
                  v-model="queryParams.paySubject"
                  placeholder="请输入材料科目"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="工资" prop="feeWages">
                <el-input
                  v-model="queryParams.feeWages"
                  placeholder="请输入工资"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="保险" prop="feeInsure">
                <el-input
                  v-model="queryParams.feeInsure"
                  placeholder="请输入保险"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="奖金" prop="feeBonus">
                <el-input
                  v-model="queryParams.feeBonus"
                  placeholder="请输入奖金"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="公积金" prop="feeAccfund">
                <el-input
                  v-model="queryParams.feeAccfund"
                  placeholder="请输入公积金"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>

          </el-row>
        </el-form>
      </template>
      <template slot="deptName" slot-scope="{scope}">
        <a @click="deptInfoUpdate(scope.row)" style="color: #1890ff">
          {{ scope.row.deptName }}
          <i class="el-icon-document-copy" title="点击复制" @click.stop="copyNodeValue($event,scope.row.deptName)"/>
        </a>
      </template>
    </jq-table>
    </el-col>
    </el-row>
    <!---企业部门信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="企业部门信息导入.xlsx"
      @handleQuery="handleQuery"
      url="/cost/deptInfo/import"
    ></import-dialog>

    <!---企业部门信息导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---企业部门信息查看详情 -->
    <deptInfo-detail :show="deptInfoCard.show" v-model="deptInfoCard.key" :currentNodeData="currentNodeData"
                    @handleClose="deptInfoClose"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import deptInfoDetail from '../common/deptInfoDetail'
  import { delDeptInfo,costDeptInfoTreeselect } from '@/api/cost/deptInfo'
  import Clipboard from 'clipboard'
  export default {
    name: "deptInfo",
    mixins: [JqTableMixin],
    provide() {
      return {
        handleQuery: this.handleQuery
      }
    },
    data() {
      return {
        deptTypeOptions: [],
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
          url: '/cost/deptInfo/list',
          method: 'get',
          queryParams: null,
          orders: 'base_dept_info.update_time desc',
          exportUrl: '/cost/deptInfo/export',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '部门名称',
            radioSearch: false,
            radioData: [{label: '搜索一', value: 1}, {label: '搜索二', value: 2}]
          }
        },
        deptInfoCard: {
          show: false,
          key: null
        },
        deptName: null,
        deptInfos:[],
        defaultProps: {
          children: 'children',
          label: 'label'
        },
        companyId: this.$store.state.item.companyId,
        itemNo:this.$store.state.item.itemNo,
        currentNodeData:{}
      }
    },
    components: {
      deptInfoDetail
    },
    created() {
      this.getDicts('dept_type').then(response => {
        this.deptTypeOptions = response.data
      })
      //console.log('this.$store.state.item.companyId',this.$store.state.item.companyId)
      this.APICostDeptInfoTreeselect();
    },
    methods: {
      /** 搜索按钮操作 */
      handleQuery() {
        this.tableConfig.queryParams = this.queryParams
        this.getJqTableData();
        this.APICostDeptInfoTreeselect();
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
        this.deptInfoCard = {
          show: true,
          key: null
        }
      },

      /** 查看企业部门信息 */
      deptInfoUpdate(row) {
        console.log('deptInfoUpdate',row,row.id)
        this.deptInfoCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭企业部门信息查看弹框 */
      deptInfoClose() {
        this.deptInfoCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除企业部门信息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delDeptInfo(ids);
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
        exportDeptInfo(queryParams);
        this.exportOpen = false;
      },

      /** 打开导入页面按钮操作 */
      handleImport() {
        this.importTitle = "节假日信息导入";
        this.importOpen = true;
      },
      filterNode(value, data) {
        if (!value) return true
        return data.label.indexOf(value) !== -1
      },
      handleNodeClick(data) {
        this.queryParams.parentId = data.id
        this.tableConfig.queryParams = this.queryParams
        this.currentNodeData=data;
        this.getJqTableData()
      },
      APICostDeptInfoTreeselect(){
        let self = this;
        costDeptInfoTreeselect({companyId:self.companyId,itemNo:self.itemNo}).then(resp=>{
          self.deptInfos = resp.data

        });
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
    },
    watch:{
      // '$store.state.item.companyId'(newVal,oldVal){
      //   this.$nextTick(()=>{
      //     this.companyId=this.$store.state.item.companyId;
      //     this.itemNo=this.$store.state.item.itemNo;
      //     this.APICostDeptInfoTreeselect();
      //   })
      // },
      deptName(val) {
        this.$refs.tree.filter(val)
      },
      '$store.state.item': {
        handler(val,val2) {
          console.log('$store.state.item',val,val2)
          this.$nextTick(()=>{
            this.companyId=this.$store.state.item.companyId;
            this.itemNo=this.$store.state.item.itemNo;
            this.APICostDeptInfoTreeselect();
          })
        },
        deep: true
      }
    }
  }
</script>
