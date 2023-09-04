<template>
  <div class="app-container">
  <el-row :gutter="10" class="table-opt mb8">
    <el-col :span="1.5">
    <el-select v-model="value" placeholder="请选择研发项目" @change="switchProject">
      <el-option
        v-for="item in allProjectList"
        :key="item.id"
        :label="`${item.projectNo} - ${item.projectName}`"
        :value="item.id">
      </el-option>
    </el-select>
    </el-col>
    <el-col :span="1.5">
      <el-button type="primary" @click="genProjectFee">
        生成研发项目辅助账
      </el-button>
    </el-col>
  </el-row>
  <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
            @handleSelectionChange="handleSelectionChange">
  </jq-table>
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>
  </div>
</template>
<script>
  import { exportAccount } from '@/api/cost/accountInfo'
  import { getAllProject } from '@/api/cost/projectInfo'
  import JqTableMixin from '@/mixin/JqTable'
  import { Loading } from 'element-ui'
  import axios from 'axios'
  import { getToken } from '@/utils/auth'

  export default {
    name: 'index',
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
        value: '',
        projectId: '' ,
        auxiliaryList:[],
        //研发项目下拉框
        allProjectList:[],
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
          url: '/cost/accountInfo/auxiliaryList',
          method: 'get',
          queryParams: null,
          orders: 'base_account_info.update_time desc',
          exportUrl: '/cost/accountInfo/exportAccount',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '',
            radioSearch: false,
            radioData: []
          },
          projectId: '' ,
        },
        accountInfoCard: {
          show: false,
          key: null
        },
        itemNo: this.$store.state.item.itemNo,
        companyId: this.$store.state.item.companyId,
      }
    },
    watch:{
      '$store.state.item.itemNo'(newVal,oldVal){
        this.$nextTick(()=>{
          this.itemNo = this.$store.state.item.itemNo;
          this.companyId = this.$store.state.item.companyId;
          this.projectList()
        })
      }
    },
    created() {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo;
        this.companyId = this.$store.state.item.companyId;
        this.projectList()
      })
    },

    methods: {
      projectList() {
        const data = {
          companyId: this.$store.state.item.companyId,
          itemNo: this.$store.state.item.itemNo,
          projectId :this.queryParams.projectId
        }
        getAllProject(data).then(res => {
          this.allProjectList = res.data;
          //设置下拉框默认选项
          if (this.allProjectList.length > 0) {
            this.value=this.allProjectList[0].id
            this.switchProject()
          }
          else{
            this.value = "";
          }
        })
      },
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
      /** 项目change事件 */
      switchProject() {
        this.queryParams.projectId = this.value;
        this.handleQuery()
      },
      /** 生成研发项目辅助账按钮 */
      genProjectFee() {
        this.$confirm('确定生成研发项目辅助账？', '提示', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(() => {
          let target = document.querySelector('.app-container')
          let loadingInstance = Loading.service({
            target: target, background: '#a6d6ff26', text: '生成中'
          })
          axios({
            method: 'get',
            url: this.$store.getters.apiHostPath + '/gen/itemFile/genProjectFee',
            headers: { 'Authorization': 'Bearer ' + getToken() },
            params: this.queryParams
          }).then(res => {
            loadingInstance.close()
            if (res.data.success == 'true') {
              window.open(process.env.VUE_APP_BASE_API + res.data.message)
            } else {
              this.msgError(res.data.message)
            }
          }).catch(e => {
            this.msgError('异常，请联系管理员！')
            loadingInstance.close()
          })
        })
/*
        this.tableData = this.$refs.JqTableRef.tableData
        if (this.tableData.length === 0) {
          this.msgError('当前页无数据数据！')
          return
        }
        this.$confirm('是否生成研发项目辅助账？', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(() => {
          this.queryParams.exportType = 1;
          this.exportOpen = true;
          this.queryParams.ids = this.ids.join(",");
          let queryParams = { ...this.queryParams, ...this.getPageSize() }
          queryParams.menuKey = this.$route.query.menuKey
          exportAccount(queryParams);
          this.exportOpen = false;
        }).then(() => {
          this.msgSuccess('生成研发项目辅助账成功！')
        }).catch(() => {
        })
*/

      },
      /** 多选框选中数据 */
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
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
        exportAccount(queryParams);
        this.exportOpen = false;
      },
    }
  }
</script>


