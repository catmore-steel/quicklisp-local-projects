<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:patentInfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-setting" @click="workProject">
          拟定研发项目
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="info" icon="el-icon-upload2" @click="handleImport"
                   v-hasPermi="['cost:patentInfo:import']">
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
              <el-form-item label="知产编号" prop="patentCode">
                <el-input
                  v-model="queryParams.patentCode"
                  placeholder="请输入知产编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="知产名称" prop="patentName">
                <el-input
                  v-model="queryParams.patentName"
                  placeholder="请输入知产名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="专利号" prop="patentNo">
                <el-input
                  v-model="queryParams.patentNo"
                  placeholder="请输入专利号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="获得方式" prop="patentGetType">
                <jq-dict-select :value.sync="queryParams.patentGetType" dict-type="patent_get_type" placeholder="请选择获得方式"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="知产状态" prop="patentStatus">
                <jq-dict-select :value.sync="queryParams.patentStatus" dict-type="patent_status" placeholder="请选择知产状态"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="知产类别" prop="patentType">
                <el-select v-model="queryParams.patentType" placeholder="请选择知产类别" class="form-control" clearable
                           filterable>
                  <el-option
                    v-for="dict in patentTypeOptions"
                    :key="dict.id"
                    :label="dict.name"
                    :value="dict.id"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="申请日期" prop="applyDate">
                <el-date-picker
                  clearable size="small"
                  v-model="queryParams.applyDate"
                  class="form-control"
                  type="date"
                  value-format="yyyy-MM-dd"
                  placeholder="选择申请日期">
                </el-date-picker>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="授权日期" prop="grantDate">
                <el-date-picker
                  clearable size="small"
                  v-model="queryParams.grantDate"
                  class="form-control"
                  type="date"
                  value-format="yyyy-MM-dd"
                  placeholder="选择授权日期">
                </el-date-picker>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="patentCode" slot-scope="{scope}">
        <a @click="patentInfoUpdate(scope.row)" v-copy>
          {{ scope.row.patentCode }}
        </a>
      </template>
    </jq-table>

    <!---企业知识产权信息导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="企业知识产权信息导入.xlsx"
      @handleQuery="handleQuery"
      :url=uploadUrl
    ></import-dialog>

    <!---企业知识产权信息导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---企业知识产权信息查看详情 -->
    <patentInfo-detail :show="patentInfoCard.show" v-model="patentInfoCard.key"
                       @handleClose="patentInfoClose" @handleQuery="handleQuery"/>

    <!--  拟定研发项目弹框  -->
    <jq-dialog title="拟定研发项目" :visible.sync="projectOpen" fullscreen append-to-body>
      <el-form ref="projectForm" :model="projectForm" :rules="projectRules" label-width="150px">
        <el-row :gutter="20" style="margin-bottom: 20px">
          <el-col :span="3">
            <span>
              当前知识产权数: {{ projectForm.patentNum }}
            </span>
          </el-col>
          <el-col :span="3">
            <span>
              年度研发总费用: {{ projectForm.devSum }} 元
            </span>
          </el-col>
          <el-col :span="3">
            <span>
              已有研发项目数: {{ projectForm.projectHaveNum }}
            </span>
          </el-col>
        </el-row>
        <jq-panel title="研发项目">
          <el-row>
            <el-col :span="8">
              <el-form-item label="拟定研发项目数" prop="replyDate">
                <el-input v-model="projectForm.projectAddNum" placeholder="请输入拟定研发项目数"/>
              </el-form-item>
            </el-col>
            <el-col :span="4" style="margin-left: 20px">
              <el-button type="primary" icon="el-icon-setting" @click="AIWork">
                AI拟定
              </el-button>
              <el-button type="primary" icon="el-icon-add" @click="addProject">
                新增
              </el-button>
            </el-col>
          </el-row>
          <el-table :data="projectForm.projectList" style="margin-bottom: 22px;">
            <el-table-column label="研发项目编号" :sortable="false" width="120">
              <template slot-scope="scope">
                <el-input v-model="scope.row.projectNo"/>
              </template>
            </el-table-column>
            <el-table-column label="研发项目名称" :sortable="false">
              <template slot-scope="scope">
                <el-input v-model="scope.row.projectName"/>
              </template>
            </el-table-column>
            <el-table-column label="技术领域" :sortable="false">
              <template slot-scope="scope">
                <treeselect v-model="scope.row.technicalCode" :options="technicalOptions" :normalizer="normalizer"  class="form-control"
                            :disable-branch-nodes="true" placeholder="请选择技术领域" :appendToBody="true"/>
              </template>
            </el-table-column>
            <el-table-column label="研发形式" :sortable="false" width="130">
              <template slot-scope="scope">
                <jq-dict-select :value.sync="scope.row.rdForm" dict-type="rd_form" placeholder="请选择研发形式"/>
              </template>
            </el-table-column>
            <el-table-column label="含委托研发" :sortable="false" width="120">
              <template slot-scope="scope">
                <el-switch
                  style="display: block; height: 25px"
                  v-model="scope.row.isEntrust"
                  active-color="#13ce66"
                  inactive-color="#ff4949"
                  active-text="不含"
                  inactive-text="含"
                  active-value="N"
                  inactive-value="Y">
                </el-switch>
              </template>
            </el-table-column>
            <el-table-column label="委托单位" :sortable="false">
              <template slot-scope="scope">
                <el-input v-model="scope.row.entrustName" :disabled="scope.row.isEntrust == 'N'"/>
              </template>
            </el-table-column>
            <el-table-column label="委托境内外" :sortable="false" width="130">
              <template slot-scope="scope">
                <el-switch :disabled="scope.row.isEntrust == 'N'"
                  style="display: block; height: 25px"
                  v-model="scope.row.isOverseas"
                  active-color="#13ce66"
                  inactive-color="#ff4949"
                  active-text="境内"
                  inactive-text="境外"
                  active-value="N"
                  inactive-value="Y">
                </el-switch>
              </template>
            </el-table-column>
            <el-table-column label="关联关系" :sortable="false" width="150">
              <template slot-scope="scope">
                <el-switch :disabled="scope.row.isEntrust == 'N'"
                  style="display: block; height: 25px"
                  v-model="scope.row.isAssociate"
                  active-color="#13ce66"
                  inactive-color="#ff4949"
                  active-text="不关联"
                  inactive-text="关联"
                  active-value="N"
                  inactive-value="Y">
                </el-switch>
              </template>
            </el-table-column>
            <el-table-column label="操作" :sortable="false" width="100">
              <template slot-scope="scope">
                <el-button type="danger" icon="el-icon-delete" @click="deleteProject(scope.$index)">
                  删除
                </el-button>
              </template>
            </el-table-column>
          </el-table>
        </jq-panel>

      </el-form>
      <div slot="footer">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="projectOpen = false">取 消</el-button>
      </div>
    </jq-dialog>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import patentInfoDetail from '../common/patentInfoDetail'
import { exportPatentInfo, getPatentTypeSelect, workProjectInfoInit, workProjectSubmit } from '@/api/cost/patentInfo'
import { listField } from '@/api/conf/field'

export default {
  name: 'patentInfo',
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
      exportTitle: '导出警告',
      // 是否显示导出弹出层
      exportOpen: false,
      //导入弹出层title
      importTitle: '导入',
      // 是否显示导出弹出层
      importOpen: false,
      // 获得方式字典
      patentGetTypeOptions: [],
      // 知产状态字典
      patentStatusOptions: [],
      //技术领域下拉数据
      technicalOptions: [],
      //研发形式字典
      rdFormOptions: [],
      // 知产类别字典
      patentTypeOptions: [],
      // 查询参数
      queryParams: {},
      //表格配置数据
      tableConfig: {
        url: '/cost/patentInfo/list',
        method: 'get',
        queryParams: null,
        orders: 'cost_patent_info.update_time desc',
        exportUrl: '/cost/patentInfo/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '知产编号/知产名称',
          radioSearch: false,
          radioData: [{ label: '搜索一', value: 1 }, { label: '搜索二', value: 2 }]
        }
      },
      patentInfoCard: {
        show: false,
        key: null
      },
      //文件上传url
      uploadUrl: '',

      projectOpen: false,
      projectForm: {
        patentNum: 0,
        devSum: 0,
        projectHaveNum: 0,
        projectAddNum: 0,
        projectList: []
      },
      projectRules: {
        replyDate: [
          { required: true, message: '交局日期不能为空', trigger: 'blur' }
        ]
      }
    }
  },
  components: {
    patentInfoDetail
  },
  created() {
    this.getDicts('patent_get_type').then(response => {
      this.patentGetTypeOptions = response.data
    })
    this.getDicts('patent_status').then(response => {
      this.patentStatusOptions = response.data
    })
    this.getDicts("rd_form").then(response => {
      this.rdFormOptions = response.data;
    });
    listField(null).then(response => {
      this.technicalOptions = this.handleTree(response.data, 'codeNumber', 'parentCodeNumber')
    })
    this.getPatentTypes()
  },
  methods: {
    /** 获取知产类别下拉数据 */
    getPatentTypes() {
      getPatentTypeSelect().then(res => {
        this.patentTypeOptions = res.data
      })
    },

    /** 转换机构数据结构 */
    normalizer(node) {
      if (node.children && !node.children.length) {
        delete node.children
      }
      return {
        id: node.codeNumber,
        label: node.title,
        children: node.children
      }
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
      this.patentInfoCard = {
        show: true,
        key: null
      }
    },

    /** 查看企业知识产权信息 */
    patentInfoUpdate(row) {
      this.patentInfoCard = {
        show: true,
        key: row.patentCode//此处id需替换为业务表唯一索引（非id）
      }
    },

    /** 关闭企业知识产权信息查看弹框 */
    patentInfoClose() {
      this.patentInfoCard = {
        show: false,
        row: null
      }
    },

    /** 拟定研发项目 */
    workProject() {
      this.projectOpen = true
      const params = {
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo
      }
      workProjectInfoInit(params).then(res => {
        this.projectForm = res.data
      })
    },

    /** AI拟定 */
    AIWork() {

    },
    /** 新增研发项目 */
    addProject() {
      this.projectForm.projectList.push({
        projectNo: '',
        projectName: '',
        technicalCode: '',
        rdForm: '',
        isEntrust: 'N',
        entrustName: '',
        isOverseas: 'N',
        isAssociate: 'N'
      })
    },

    /** 删除研发项目 */
    deleteProject(index){
      this.projectForm.projectList.splice(index,1)
    },

    /** 拟定研发项目提交 */
    submitForm() {
      workProjectSubmit(this.projectForm).then(res=>{
        this.projectOpen = false
        this.msgSuccess(res.msg)
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
      exportPatentInfo(queryParams)
      this.exportOpen = false
    },

    /** 打开导入页面按钮操作 */
    handleImport() {
      this.uploadUrl = '/cost/patentInfo/import?companyId=' + this.$store.state.item.companyId + '&itemNo=' + this.$store.state.item.itemNo
      this.importTitle = '节假日信息导入'
      this.importOpen = true
    }
  }
}
</script>

