<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px">
      <jq-panel title="可参与研发人员明细">
        <el-row v-if="feeTypeName !== '无形资产投入'">
          <el-col :span="12">
            <el-form-item label="研发阶段" prop="id">
              <el-select v-model="detail.id" placeholder="请选择项目阶段" clearable filterable @change="changeStage"
                         style="width: 250px;">
                <el-option
                  v-for="dict in projectStagesOptions"
                  :key="dict.id"
                  :label="dict.stageName"
                  :value="dict.id"
                ></el-option>
              </el-select>
              <span>
                阶段起止时间：{{ stageStartEndDate }}
              </span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-table :data="detail.userFeeList" :key="tableKey" :span-method="objectSpanMethod" show-summary
                    v-if="feeTypeName == '人员投入'">
            <el-table-column label="阶段月度" :sortable="false" prop="month"/>
            <el-table-column label="可参与研发人员" :sortable="false" prop="userName" width="150"/>
            <el-table-column label="人员类型" :sortable="false" prop="userTypeName"/>
            <el-table-column label="部门" :sortable="false" prop="deptName"/>
            <el-table-column label="当月投入研发" :sortable="false" prop="devHours" width="120"/>
            <el-table-column label="当月已投入其他研发项目" :sortable="false" prop="haveUserHours" width="200"/>
            <el-table-column label="当月可投入研发" :sortable="false" prop="restHours" width="120"/>
            <el-table-column prop="thisTimeHours" label="本次投入" width="200">
              <template slot="header" slot-scope="scope">
                本次投入<el-button type="primary" @click="share">一键分摊</el-button>
              </template>
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeHours" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeSalaryFee" label="本次投入人员费用（工资）" width="200">
              <template slot-scope="scope">
                <el-input readonly v-model="scope.row.thisTimeSalaryFee"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeSafeFee" label="本次投入人员费用（社保）" width="200">
              <template slot-scope="scope">
                <el-input readonly v-model="scope.row.thisTimeSafeFee"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeProvidentFee" label="本次投入人员费用（公积金）" width="210">
              <template slot-scope="scope">
                <el-input readonly v-model="scope.row.thisTimeProvidentFee"/>
              </template>
            </el-table-column>
          </el-table>

          <el-table :data="detail.materialFeeList" :key="tableKey" :span-method="objectSpanMethod" show-summary
                    v-if="feeTypeName == '直接投入'">
            <el-table-column label="阶段月度" :sortable="false" prop="month"/>
            <el-table-column label="直接投入类型" :sortable="false" prop="directTypeName"/>
            <el-table-column label="当月投入研发" :sortable="false" prop="devFee"/>
            <el-table-column label="当月已投入其他研发项目" :sortable="false" prop="haveUseFee" width="200"/>
            <el-table-column label="当月可投入研发" :sortable="false" prop="restFee"/>
            <el-table-column prop="thisTimeFee">
              <template slot="header" slot-scope="scope">
                本次投入研发<el-button type="primary" @click="share">一键分摊</el-button>
              </template>
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeFee" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
          </el-table>

          <el-table :data="detail.deviceFeeList" :key="tableKey" :span-method="objectSpanMethod" show-summary
                    v-if="feeTypeName == '设备折旧'">
            <el-table-column label="阶段月度" :sortable="false" prop="month"/>
            <el-table-column label="可参与设备" :sortable="false" prop="deviceName"/>
            <el-table-column label="设备类型" :sortable="false" prop="deviceTypeName"/>
            <el-table-column label="当月投入研发" :sortable="false" prop="devHours"/>
            <el-table-column label="当月已投入其他研发项目" :sortable="false" prop="haveUseHours" width="200"/>
            <el-table-column label="当月可投入研发" :sortable="false" prop="restHours"/>
            <el-table-column prop="thisTimeHours">
              <template slot="header" slot-scope="scope">
                本次投入研发<el-button type="primary" @click="share">一键分摊</el-button>
              </template>
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeHours" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeFee" label="本次设备折旧费用">
              <template slot-scope="scope">
                <el-input readonly v-model="scope.row.thisTimeFee"/>
              </template>
            </el-table-column>
          </el-table>

          <el-table :data="detail.ipFeeList" :key="tableKey" show-summary v-if="feeTypeName == '无形资产投入'">
            <el-table-column label="无形资产名称" :sortable="false" prop="ipName"/>
            <el-table-column label="资产类型" :sortable="false" prop="ipTypeName"/>
            <el-table-column label="可投入研发摊销月数" :sortable="false" prop="devMonths"/>
            <el-table-column prop="thisTimeMonthArray" label="本次投入研发">
              <template slot-scope="scope">
                <el-select v-model="scope.row.thisTimeMonthArray" placeholder="请选择月份，已被使用的月份不能再选择" class="form-control" clearable
                           filterable multiple @change="checkThisTimeHours(scope.row)">
                  <el-option
                    v-for="dict in monthOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeFee" label="本次无形资产摊销费用">
              <template slot-scope="scope">
                <el-input readonly v-model="scope.row.thisTimeFee"/>
              </template>
            </el-table-column>
          </el-table>

          <el-table :data="detail.designFeeList" :key="tableKey" :span-method="objectSpanMethod" show-summary
                    v-if="feeTypeName == '新品设计投入'">
            <el-table-column label="阶段月度" :sortable="false" prop="month"/>
            <el-table-column label="当月投入研发" :sortable="false" prop="devFee"/>
            <el-table-column label="当月已投入其他研发项目" :sortable="false" prop="haveUseFee" width="200"/>
            <el-table-column label="当月可投入研发" :sortable="false" prop="restFee"/>
            <el-table-column prop="thisTimeFee" label="本次新产品设计费">
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeFee" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeFeeTwo" label="本次新工艺规程制定费">
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeFeeTwo" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeFeeThree" label="本次新药研制的临床试验费">
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeFeeThree" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
            <el-table-column prop="thisTimeFeeFour" label="本次勘探开发技术的现场试验费" width="230">
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeFeeFour" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
          </el-table>

          <el-table :data="detail.otherFeeList" :key="tableKey" :span-method="objectSpanMethod" show-summary
                    v-if="feeTypeName == '其他费用投入'">
            <el-table-column label="阶段月度" :sortable="false" prop="month"/>
            <el-table-column label="当月投入研发" :sortable="false" prop="devFee"/>
            <el-table-column label="当月已投入其他研发项目" :sortable="false" prop="haveUseFee" width="200"/>
            <el-table-column label="当月可投入研发" :sortable="false" prop="restFee"/>
            <el-table-column prop="thisTimeFee">
              <template slot="header" slot-scope="scope">
                本次新产品设计费<el-button type="primary" @click="share">一键分摊</el-button>
              </template>
              <template slot-scope="scope">
                <el-input v-model="scope.row.thisTimeFee" @input="checkThisTimeHours(scope.row)"/>
              </template>
            </el-table-column>
          </el-table>
        </el-row>
      </jq-panel>
    </el-form>
  </div>
</template>

<script>
import { getProjectStageFeeList, getProjectStagesSelect, oneKeyShare, submitFeeShare } from '@/api/cost/projectInfo'
import { isNullOrEmpty } from '@/utils/jq'

export default {
  name: 'projectFeeShare',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      detail: {},
      //研发阶段字典
      projectStagesOptions: [],
      tableKey: 0,
      //表单校验
      rules: {
        projectName: [
          { required: true, message: '项目名称不能为空', trigger: 'blur' }
        ]
      },

      //阶段起止日期
      stageStartEndDate: '',

      //月份字典数据
      monthOptions: [],
    }
  },
  props: {
    projectId: {
      type: Number
    },
    feeTypeName: {
      type: String
    }
  },

  created() {
    this.getDicts('month').then(response => {
      this.monthOptions = response.data
    })
    this.detail = {}
    this.getProjectStagesSelect()
    this.detail.companyId = this.$store.state.item.companyId
    this.detail.itemNo = this.$store.state.item.itemNo
    this.detail.feeTypeName = this.feeTypeName
    this.detail.projectId = this.projectId
  },
  methods: {

    /** 获取研发项目阶段下拉 */
    getProjectStagesSelect() {
      const that = this
      getProjectStagesSelect(this.projectId).then(res => {
        that.projectStagesOptions = res.data
        that.detail.id = res.data[0].id
        that.stageStartEndDate = res.data[0].startDate + '至' + res.data[0].endDate
        that.getProjectStageFeeList()
      })
    },

    /** 获取研发项目管理详情 */
    getProjectStageFeeList() {
      const feeTypeName = this.feeTypeName
      getProjectStageFeeList(this.detail).then(res => {
        if (feeTypeName == '人员投入') {
          this.detail.userFeeList = res.data
        } else if (feeTypeName == '直接投入') {
          this.detail.materialFeeList = res.data
        } else if (feeTypeName == '设备折旧') {
          this.detail.deviceFeeList = res.data
        } else if (feeTypeName == '无形资产投入') {
          this.detail.ipFeeList = res.data
          this.detail.ipFeeList.forEach(item=>{
            if(!isNullOrEmpty(item.thisTimeMonths)){
              item.thisTimeMonthArray = item.thisTimeMonths.split(',')
            }
          })
        } else if (feeTypeName == '其他费用投入') {
          this.detail.otherFeeList = res.data
        } else if (feeTypeName == '新品设计投入') {
          this.detail.designFeeList = res.data
        }
        this.tableKey = this.tableKey + 1
      })
    },

    /** 项目阶段change事件 */
    changeStage(val) {
      let text = ''
      this.projectStagesOptions.filter(item => {
        if (item.id == val) {
          text = item.startDate + '至' + item.endDate
        }
      })
      this.stageStartEndDate = text
      this.detail.id = val
      const feeTypeName = this.feeTypeName
      if (feeTypeName == '人员投入') {
        this.detail.userFeeList = []
      } else if (feeTypeName == '直接投入') {
        this.detail.materialFeeList = []
      } else if (feeTypeName == '设备折旧') {
        this.detail.deviceFeeList = []
      } else if (feeTypeName == '其他费用投入') {
        this.detail.otherFeeList = []
      } else if (feeTypeName == '新品设计投入') {
        this.detail.designFeeList = []
      }

      this.getProjectStageFeeList()
    },

    /** 合并阶段月度行数据 */
    objectSpanMethod({ row, column, rowIndex, columnIndex }) {
      if (columnIndex === 0) {
        const feeTypeName = this.feeTypeName
        let tableDateList = []
        if (feeTypeName == '人员投入') {
          tableDateList = this.detail.userFeeList
        } else if (feeTypeName == '直接投入') {
          tableDateList = this.detail.materialFeeList
        } else if (feeTypeName == '设备折旧') {
          tableDateList = this.detail.deviceFeeList
        } else if (feeTypeName == '其他费用投入') {
          tableDateList = this.detail.otherFeeList
        } else if (feeTypeName == '新品设计投入') {
          tableDateList = this.detail.designFeeList
        }
        // 获取当前单元格的值
        const currentValue = row[column.property]
        if (rowIndex > 0) {
          // 判断是不是第一行
          if (tableDateList[rowIndex][column.property] != tableDateList[rowIndex - 1][column.property]) {
            // 先判断当前单元格的值是不是和上一行的值相等
            let i = rowIndex
            let num = 0 // 定义一个变量i，用于记录行索引值并进行循环，num用于计数
            while (i < tableDateList.length) {
              // 当索引值小于table的数组长度时，循环执行
              if (tableDateList[i][column.property] === currentValue) {
                // 判断循环的单元格的值是不是和当前行的值相等
                i++ // 如果相等，则索引值加1
                num++ // 合并的num计数加1
              } else {
                i = tableDateList.length // 如果不相等，将索引值设置为table的数组长度，跳出循环
              }
            }
            return {
              rowspan: num, // 最终将合并的行数返回
              colspan: 1
            }
          } else {
            return {
              rowspan: 0, // 如果相等，则将rowspan设置为0
              colspan: 1
            }
          }
        } else {
          // 如果是第一行，则直接返回
          let i = rowIndex
          let num = 0
          while (i < tableDateList.length) {
            // 当索引值小于table的数组长度时，循环执行
            if (tableDateList[i][column.property] === currentValue) {
              i++
              num++
            } else {
              i = tableDateList.length
            }
          }
          return {
            rowspan: num,
            colspan: 1
          }
        }
      }
    },

    /** 一键分摊 */
    share(){
      const feeTypeName = this.feeTypeName
      this.detail.userFeeList = []
      this.detail.materialFeeList = []
      this.detail.deviceFeeList = []
      this.detail.otherFeeList = []
      oneKeyShare(this.detail).then(res=>{
        if (feeTypeName == '人员投入') {
          this.detail.userFeeList = res.data
        } else if (feeTypeName == '直接投入') {
          this.detail.materialFeeList = res.data
        } else if (feeTypeName == '设备折旧') {
          this.detail.deviceFeeList = res.data
        } else if (feeTypeName == '其他费用投入') {
          this.detail.otherFeeList = res.data
        }
        this.tableKey = this.tableKey + 1
      })

    },

    /** 本次投入工时校验，不超过当月可投入研发 */
    checkThisTimeHours(row) {
      const feeTypeName = this.feeTypeName
      if (feeTypeName == '人员投入') {
        if (row.thisTimeHours > row.restHours) {
          this.msgError('本次投入不能超过当月可投入研发')
          row.thisTimeHours = 0
          row.thisTimeSalaryFee = 0
          row.thisTimeSafeFee = 0
          row.thisTimeProvidentFee = 0
          return false
        }
        //工资
        row.thisTimeSalaryFee = (row.thisTimeHours / row.devHours * row.devSalaryFee).toFixed(2)
        //社保
        row.thisTimeSafeFee = (row.thisTimeHours / row.devHours * row.devSocialFee).toFixed(2)
        //公积金
        row.thisTimeProvidentFee = (row.thisTimeHours / row.devHours * row.devProvidentFee).toFixed(2)
      } else if (feeTypeName == '直接投入') {
        if (row.thisTimeFee > row.restFee) {
          this.msgError('本次投入不能超过当月可投入研发')
          row.thisTimeFee = 0
          return false
        }
      } else if (feeTypeName == '设备折旧') {
        if (row.thisTimeHours > row.restHours) {
          this.msgError('本次投入不能超过当月可投入研发')
          row.thisTimeHours = 0
          row.thisTimeFee = 0
          return false
        }
        row.thisTimeFee = (row.thisTimeHours / row.devHours * row.devFee).toFixed(2)
      } else if (feeTypeName == '无形资产投入') {
        if(row.thisTimeMonthArray.length > row.devMonths){
          this.msgError('本次投入不能超过当月可投入研发')
          row.thisTimeMonthArray = []
          row.thisTimeFee = 0
          return false
        }
        row.thisTimeFee = (row.thisTimeMonthArray.length / row.devMonths * row.devFee).toFixed(2)
      } else if (feeTypeName == '其他费用投入') {
        if(row.thisTimeFee > row.restFee){
          this.msgError('本次投入不能超过当月可投入研发')
          row.thisTimeFee = 0
          return false
        }
      } else if (feeTypeName == '新品设计投入') {
        row.thisTimeFeeTotal = Number(row.thisTimeFee) +  Number(row.thisTimeFeeTwo) + Number(row.thisTimeFeeThree) + Number(row.thisTimeFeeFour)
        if(row.thisTimeFeeTotal > Number(row.restFee)){
          this.msgError('本次投入不能超过当月可投入研发')
          row.thisTimeFee = 0
          row.thisTimeFeeTwo = 0
          row.thisTimeFeeThree = 0
          row.thisTimeFeeFour = 0
          return false
        }
      }

    },

    /** 提交按钮 */
    submitFeeShare() {
      submitFeeShare(this.detail).then(response => {
        this.msgSuccess(response.msg)
        this.$emit('handleClose')
      })
    }
  }
}
</script>
