<template>
  <div class="app-container">
    <jq-panel title="一、初始化配置">
      <el-form ref="refForm" :model="form" :rules="rules">
        <el-row>
          <el-col :span="6">
            <el-form-item label="考勤类型：" prop="workingType" label-width="300">
              <jq-dict-select :value.sync="form.workingType" dict-type="working_type" select-style="width: 200px"/>
            </el-form-item>
          </el-col>
          <el-col :span="6">
            <el-form-item label="工作日标准工时：" prop="workingHour" label-width="300">
              <el-input v-model.number="form.workingHour" style="width: 200px"
                        @input="val => form.workingHour = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
              />
            </el-form-item>
          </el-col>
          <el-col :span="6">
            <el-form-item>
              <el-button type="primary" icon="el-icon-edit" @click="saveOrUpdate">一键获取排班计划</el-button>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
    </jq-panel>
    <!-- 排班计划 -->
    <jq-panel title="二、排班计划明细">
      <div style="margin: 20px 0">
        <span>{{ form.year }} 年度排班计划</span>
      </div>
      <div class="grid_box" v-if="!isNullOrEmpty(form) && !isNullOrEmpty(form.budgetWorkingSumVoList)">
        <div v-for="(item, index) in form.budgetWorkingSumVoList" class="item" @click="handlePlan(item.month)">
          <div style="font-weight: bold">{{ item.month }} 月</div>
          <div style="display: flex; justify-content: space-between; margin: 10px 0; color: #333;">
            <span>工作天数: {{ item.ban }} 天</span>
            <span>标准工作时长: {{ item.workingHour }} h</span>
          </div>
          <div style="display: flex; justify-content: space-between; color: #333">
            <span>休息天数: {{ item.xiu }} 天</span>
            <span>月工作总时长: {{ item.ban * item.workingHour }} h</span>
          </div>
        </div>
      </div>
      <div v-else>
        <span>暂未配置排班计划</span>
      </div>
    </jq-panel>

    <!-- 每月排班明细 -->
    <jq-dialog
      :title="scheduleTitle"
      :visible.sync="scheduleOpen"
      fullscreen
      append-to-body
      :before-close="handleClose"
    >
      <div>
        <el-form>
          <el-row>
            <el-col :span="4">
              <el-form-item label-width="300" label="设定休息日:">
                <el-switch
                  v-model="xiu"
                  @change="
                    (val) => {
                      if (val) {
                        this.ban = false;
                        this.active = 'xiu';
                      } else {
                        this.xiu = false;
                        this.ban = true;
                        this.active = 'ban';
                      }
                    }
                  "
                />
              </el-form-item>
            </el-col>
            <el-col :span="4">
              <el-form-item label-width="300" label="设定工作日:">
                <el-switch
                  v-model="ban"
                  @change="(val) => {if (val) {
                    this.xiu = false;
                    this.active = 'ban';
                  } else {
                    this.ban = false;
                    this.xiu = true
                    this.active = 'xiu';
                  }}"
                />
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>

        <div class="rili">
          <!-- 星期 -->
          <ul class="week_day">
            <li>日</li>
            <li>一</li>
            <li>二</li>
            <li>三</li>
            <li>四</li>
            <li>五</li>
            <li>六</li>
          </ul>
          <!-- 日期 -->
          <ul class="day_body" @click="dayClick($event)">
            <li v-for="item in compDay.sday"></li>
            <li
              v-for="(item, index) in budgetWorkingDetailList"
              :class="['item', item.workingType == 0? '': item.workingType == 1? 'xiu' : 'ban',]"
              :key="form.year + '/' + (activeMonth - 1) + '/' + index"
            >
              {{ index + 1 }}
            </li>
          </ul>
        </div>
      </div>
      <span slot="footer" class="dialog-footer" style="text-align: center">
          <el-button type="primary" @click="save">保存</el-button>
          <el-button type="info" @click="handleClose()">返回</el-button>
      </span>
    </jq-dialog>

  </div>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { getWorkingDetailByMonth, getWorkingInfo, saveLegalHoliday, updateWorkingDetail } from '@/api/cost/workinginfo'

export default {
  data() {
    return {
      form: {
        // 年份
        year: 1970,
        //月份数组
        budgetWorkingSumVoList: [],
        workingType: null,
        workingHour: null
      },
      //点击日历的月份
      activeMonth: 0,
      //工作日标准工时数字大小检验
      rules: {
        workingHour: [
          {
            validator: (rules, value, callback) => {
              if (value < 0 || value > 24) {
                return callback(new Error('工时在 0 到 24 之间'))
              } else {
                callback()
              }
            }
          }
        ]
      },

      //每月排班详情是否显示弹出层
      scheduleOpen: false,
      //每月排班详情弹出层title
      scheduleTitle: '',
      //月排班明细数组
      budgetWorkingDetailList: [],
      xiu: true,
      ban: false,
      active: ''
    }
  },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.getMonthList()
      })
    }
  },
  mounted() {
    if (!isNullOrEmpty(this.$store.state.item.itemNo)) {
      this.getMonthList()
    }
  },
  computed: {
    compDay() {
      return {
        sday: new Date(this.form.year, this.activeMonth - 1, 1).getDay() || 0
      }
    }
  },
  methods: {
    //获取每月排班和休息信息
    getMonthList() {
      this.form = {}
      getWorkingInfo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(res => {
          this.form = res.data
        }
      )
    },

    /** 首页的【保存设置】按钮操作 */
    saveOrUpdate() {
      if (isNullOrEmpty(this.form.workingType)) {
        this.$message.error('请选择考勤类型！')
        return
      }
      if (isNullOrEmpty(this.form.workingHour)) {
        this.$message.error('请填写工作日标准工时！')
        return
      }

      this.$refs.refForm.validate(valid => {
        if (valid) {
          this.form.companyId = this.$store.state.item.companyId
          this.form.itemNo = this.$store.state.item.itemNo
          saveLegalHoliday(this.form).then((response) => {
            if (response.code === 200) {
              this.msgSuccess(response.msg)
              this.scheduleOpen = false
              this.getMonthList()
            }
          })
        }
      })
    },

    /** 点击排班计划的某一月 */
    handlePlan(month) {
      this.scheduleTitle = this.form.year + '年' + month + '排班'
      this.activeMonth = Number(month)
      this.scheduleOpen = true
      this.budgetWorkingDetailList = []
      getWorkingDetailByMonth(this.$store.state.item.companyId, this.$store.state.item.itemNo, this.form.year + '-' + month).then(res => {
        this.budgetWorkingDetailList = res.data
      })
    },

    /** 选中日历的具体某一天 */
    dayClick(e) {
      if (!e.target.className.includes('item')) {
        return
      }
      if (this.xiu) {
        this.budgetWorkingDetailList[e.target.innerText - 1].workingType = 1
      }
      if (this.ban) {
        this.budgetWorkingDetailList[e.target.innerText - 1].workingType = 2
      }
    },

    /** 保存按钮操作 */
    save() {
      let index = this.budgetWorkingDetailList.findIndex((item) => !item.workingType)
      //判断是否全部排好
      if (index === -1) {
        updateWorkingDetail(this.budgetWorkingDetailList).then((response) => {
          if (response.code === 200) {
            this.msgSuccess(response.msg)
            this.scheduleOpen = false
            this.getMonthList()
          }
        })
      } else {
        this.$message.error('请确保每天设置了工作和休息！')
      }
    },

    /** 返回按钮操作 */
    handleClose(done) {
      this.scheduleOpen = false
      // this.$confirm('请确定当前页面的内容已保存', '提示', {
      //   confirmButtonText: '确定',
      //   cancelButtonText: '取消',
      //   type: 'warning'
      // }).then(() => {
      //   done ? done() : (this.scheduleOpen = false)
      // })
    }
  }
}
</script>
<style scoped lang="scss">
.grid_box {
  display: grid;
  grid-template-columns: repeat(4, auto);
  grid-template-rows: repeat(3, auto);
  grid-gap: 20px;
  font-size: 14px;

  .item {
    border-top: 4px solid #ddd;
    padding-top: 10px;
    cursor: pointer;
  }
}

.rili {
  //border: 2px solid #eee;
  //height: 400px;
  position: relative;
  font-weight: bold;
  //box-shadow: -2px -2px 20px #eee;
  border-radius: 4px;
  padding: 5px;

  ul {
    margin: 0;
    padding: 0;
  }

  li {
    list-style: none;
    //background-color: pink;
    height: 75px;
    line-height: 75px;
    //width: 40px;
    text-align: center;
  }

  .week_day {
    display: grid;
    grid-template-columns: repeat(7, 1fr);
    grid-gap: 10px;
    color: #666;
    border-bottom: 1px solid #ddd;
  }

  .day_body {
    margin-top: 10px;
    display: grid;
    height: 400px;
    grid-template-columns: repeat(7, 1fr);
    grid-gap: 10px;
    color: #777;

    .item {
      //border-radius: 50px;
      //background-color: #ccc;
      cursor: pointer;

      &:hover {
        background-color: #eee;
      }
    }

    .today {
      //background-color: #409EFF;
      //color: #fff;
      &:after {
        content: "today";
        font-size: 12px;
        float: right;
        margin-top: -24px;
        color: #409eff;
      }
    }

    .ban {
      position: relative;

      &:after {
        position: absolute;
        top: -10px;
        right: 30%;
        content: "班";
        font-size: 16px;
        color: #f56c6c;
      }
    }

    .xiu {
      position: relative;

      &:after {
        position: absolute;
        top: -10px;
        right: 30%;
        content: "休";
        font-size: 16px;
        color: #67c23a;
      }
    }
  }

  .watermark {
    z-index: -1;
    position: absolute;
    font-size: 120px;
    left: 50%;
    transform: translateX(-50%);
    line-height: 300px;
    color: rgba(0, 0, 0, 0.1);
  }
}
</style>
