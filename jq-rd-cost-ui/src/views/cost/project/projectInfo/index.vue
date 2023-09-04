<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:projectInfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="setProjectStates"
                   v-hasPermi="['cost:projectInfo:add']">
          定制研发项目周期
        </el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
              @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="项目编号" prop="projectNo">
                <el-input
                  v-model="queryParams.projectNo"
                  placeholder="请输入项目编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="项目名称" prop="projectName">
                <el-input
                  v-model="queryParams.projectName"
                  placeholder="请输入项目名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="研发形式" prop="rdForm">
                <jq-dict-select :value.sync="queryParams.rdForm" dict-type="rd_form" placeholder="请选择研发形式"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="技术领域" prop="technicalCode">
                <treeselect v-model="queryParams.technicalCode" :options="technicalOptions" :normalizer="normalizer"
                            class="form-control"
                            :disable-branch-nodes="true" placeholder="请选择技术领域"/>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="是否委托研发" prop="isEntrust">
                <jq-dict-select :value.sync="queryParams.isEntrust" dict-type="sys_yes_no"
                                placeholder="请选择是否委托研发"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="是否关联" prop="isAssociate">
                <jq-dict-select :value.sync="queryParams.isAssociate" dict-type="sys_yes_no"
                                placeholder="请选择是否关联"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="是否委托境外" prop="isOverseas">
                <jq-dict-select :value.sync="queryParams.isOverseas" dict-type="sys_yes_no"
                                placeholder="请选择是否委托境外"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="委托单位" prop="entrustName">
                <el-input
                  v-model="queryParams.entrustName"
                  placeholder="请输入委托单位"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="projectNo" slot-scope="{scope}">
        <a @click="projectInfoUpdate(scope.row)" v-copy>
          {{ scope.row.projectNo }}
        </a>
      </template>
    </jq-table>

    <!---研发项目管理导入组件 -->
    <import-dialog
      :title="importTitle"
      :show.sync="importOpen"
      import-file-name="研发项目管理导入.xlsx"
      @handleQuery="handleQuery"
      url="/cost/projectInfo/import"
    ></import-dialog>

    <!---研发项目管理导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---研发项目管理查看详情 -->
    <projectInfo-detail :show="projectInfoCard.show" v-model="projectInfoCard.key"
                        @handleClose="projectInfoClose" @handleQuery="handleQuery"/>

    <!--  制定研发项目周期  -->
    <jq-dialog title="制定研发项目周期" :visible.sync="projectStageOpen" fullscreen append-to-body>
      <el-form ref="projectStageForm" :model="projectStageForm" :rules="projectStageRules" label-width="150px">
        <jq-panel title="年度研发项目周期">
          <div ref="projectStatesChart" style="width: 100%; height: 300px;"></div>
        </jq-panel>
        <jq-panel title="研发项目">
          <el-row style="margin-bottom: 20px;">
            <div>
              <el-radio-group v-model="radio" @change="getProject">
                <el-radio-button v-for="dict in projectList" :label="dict.projectNo"></el-radio-button>
              </el-radio-group>
            </div>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="项目起止日期" prop="projectStartEndDateRange">
                <el-date-picker
                  v-model="projectStageForm.projectStartEndDateRange"
                  type="monthrange"
                  range-separator="至"
                  start-placeholder="开始日期"
                  value-format="yyyy-MM"
                  end-placeholder="结束日期">
                </el-date-picker>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="项目名称" prop="replyDate">
                <span>
                  {{ projectStageForm.projectName }}
                </span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-button type="primary" icon="el-icon-add" @click="addProjectStage" style="float: right">
                新增阶段
              </el-button>
            </el-col>
          </el-row>
          <el-table :data="projectStageForm.costProjectStageList" style="margin-bottom: 22px;" max-height="200">
            <el-table-column label="项目阶段" :sortable="false">
              <template slot-scope="scope">
                <el-input v-model="scope.row.stageName"/>
              </template>
            </el-table-column>
            <el-table-column label="起始日期" :sortable="false">
              <template slot-scope="scope">
                <el-date-picker
                  v-model="scope.row.startDateDay"
                  type="month"
                  placeholder="选择日期">
                </el-date-picker>
              </template>
            </el-table-column>
            <el-table-column label="结束日期" :sortable="false">
              <template slot-scope="scope">
                <el-date-picker
                  v-model="scope.row.endDateDay"
                  type="month"
                  placeholder="选择日期">
                </el-date-picker>
              </template>
            </el-table-column>
            <el-table-column label="操作" :sortable="false" width="200">
              <template slot-scope="scope">
                <el-button type="danger" icon="el-icon-delete" @click="deleteProjectStage(scope.$index)">
                  删除
                </el-button>
              </template>
            </el-table-column>
          </el-table>
        </jq-panel>
      </el-form>
      <div slot="footer">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="projectStageOpen = false">取 消</el-button>
      </div>
    </jq-dialog>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import projectInfoDetail from '../common/projectInfoDetail'
import {
  delProjectInfo,
  getProjectByCompanyIdItemNo,
  getProjectInfoByCompanyIdItemNoProjectNo,
  projectStagesSubmit
} from '@/api/cost/projectInfo'
import { dateTypeFormat, getFirstDayOfYear, isNullOrEmpty } from '@/utils/jq'
import { listField } from '@/api/conf/field'

export default {
  name: 'projectInfo',
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
      //技术领域下拉数据
      technicalOptions: [],
      // 查询参数
      queryParams: {},
      //表格配置数据
      tableConfig: {
        url: '/cost/projectInfo/list',
        method: 'get',
        queryParams: null,
        orders: 'cost_project_info.update_time desc',
        exportUrl: '/cost/projectInfo/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '项目编号/项目名称',
          radioSearch: false,
          radioData: [{ label: '搜索一', value: 1 }, { label: '搜索二', value: 2 }]
        }
      },
      projectInfoCard: {
        show: false,
        key: null
      },

      radio: '',
      projectStageOpen: false,
      projectStageForm: {
        costProjectStageList: []
      },
      projectStageRules: {},
      //项目编号radio数据
      projectList: []
    }
  },
  components: {
    projectInfoDetail
  },
  created() {
    /** 获取技术领域下拉数据 */
    listField(null).then(response => {
      this.technicalOptions = this.handleTree(response.data, 'codeNumber', 'parentCodeNumber')
    })
  },
  methods: {
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
      this.projectInfoCard = {
        show: true,
        key: null
      }
    },

    /** 查看研发项目管理 */
    projectInfoUpdate(row) {
      this.projectInfoCard = {
        show: true,
        key: row.projectNo//此处id需替换为业务表唯一索引（非id）
      }
    },

    /** 关闭研发项目管理查看弹框 */
    projectInfoClose() {
      this.projectInfoCard = {
        show: false,
        row: null
      }
    },

    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids
      this.$confirm('是否确认删除研发项目管理编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delProjectInfo(ids)
      }).then(() => {
        this.getJqTableData()
        this.msgSuccess('删除成功')
      }).catch(() => {
      })
    },

    /** 定制研发项目周期 */
    setProjectStates() {
      this.projectStageOpen = true
      const companyId = this.$store.state.item.companyId
      const itemNo = this.$store.state.item.itemNo
      this.initPatentTypeChart(companyId, itemNo)
    },

    /** radio切换获取研发项目 */
    getProject(projectNo) {
      const data = {
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo,
        projectNo: projectNo
      }
      getProjectInfoByCompanyIdItemNoProjectNo(data).then(response => {
        this.projectStageForm = response.data
      })
    },

    /** 初始化专利类型柱状图 */
    initPatentTypeChart(companyId, itemNo) {
      const that = this
      //等dom加载完毕在获取
      this.$nextTick(() => {
        let myChart = this.$echarts.init(this.$refs.projectStatesChart)
        const backgroundColor = '#ffffff' // 全局背景颜色

        //每个项目的起止日期
        let projectMap = {}

        //获取客户项目下的研发项目信息
        getProjectByCompanyIdItemNo(companyId, itemNo).then(res => {
          that.projectList = res.data
          let projectNoList = []
          let startDateList = []
          let endDateList = []
          if (that.projectList) {
            that.projectList.forEach(function(item, i) {
              if (i == 0) {
                that.radio = item['projectNo']
              }
              projectNoList.push(item['projectNo'])
              startDateList.push(item['startDateDay'])
              endDateList.push(item['endDateDay'])
              projectMap[item['projectNo']] = { startTime: item['startDateDay'], endTime: item['endDateDay'] }
            })

            that.getProject(that.radio)

            //获取当前年第一天和最后一天
            const yearFirstLastDay = getFirstDayOfYear(new Date(), 'year')

            myChart.setOption({
              backgroundColor: backgroundColor,
              legend: {
                data: []
              },
              grid: {
                containLabel: true,
                left: 30,
                right: 30,
                top: 50,
                //上边框距离
                bottom: 50,
                show: true
              },
              xAxis: {
                type: 'time',
                //开始时间
                min: yearFirstLastDay[0],
                //结束时间
                max: yearFirstLastDay[1],
                axisLabel: {
                  interale: 0,
                  formatter: function(value) {//在这里写你需要的时间格式
                    const t_date = new Date(value)
                    return [t_date.getFullYear(), t_date.getMonth() + 1].join('-')
                  }
                }
              },
              yAxis: {
                data: projectNoList,
                splitLine: {
                  show: false
                }
              },
              tooltip: {
                trigger: 'axis',
                axisPointer: {
                  type: 'shadow'
                },
                formatter: function(params) {
                  let res = params[0].name + '</br>'
                  const start_date = new Date(params[1].data)
                  const end_date = new Date(params[0].data)
                  const startDateStr = dateTypeFormat(start_date, 'YYYY-MM')
                  const endDateStr = dateTypeFormat(end_date, 'YYYY-MM')
                  res += startDateStr + '至' + endDateStr
                  return res
                }
              },
              series: [
                {
                  name: 'endDateDay',
                  type: 'bar',
                  stack: 'test2',
                  barWidth: 20,
                  label: {
                    normal: {
                      show: true,
                      position: 'insideRight'
                    }
                  },
                  itemStyle: {
                    normal: {
                      color: function() {
                        return '#' + Math.floor(Math.random() * (256 * 256 * 256 - 1)).toString(16)
                      }
                    }
                  },
                  zlevel: -1,
                  data: endDateList //endDate
                },
                {
                  name: 'startDateDay',
                  type: 'bar',
                  stack: 'test2',
                  itemStyle: {
                    normal: {
                      color: backgroundColor,
                      borderColor: backgroundColor
                    }
                  },
                  zlevel: -1,
                  data: startDateList
                }]
            })
          }
        })
      })
    },

    /** 添加项目阶段 */
    addProjectStage() {
      if (isNullOrEmpty(this.projectStageForm.costProjectStageList)) {
        this.projectStageForm.costProjectStageList = []
      }
      this.projectStageForm.costProjectStageList.push({
        stageName: '',
        startDateDay: null,
        endDateDay: null
      })
    },

    /** 删除项目阶段数据 */
    deleteProjectStage(index) {
      this.projectStageForm.costProjectStageList.splice(index, 1)
    },

    /** 项目阶段数据提交 */
    submitForm() {
      projectStagesSubmit(this.projectStageForm).then(res => {
        this.projectStageOpen = false
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
      exportProjectInfo(queryParams)
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
