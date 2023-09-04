<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[userSalaryForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['cost:userSalary:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:userSalary:remove']">
          撤销
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="企业人员工资记录;" name="1">
            <userSalary-update :userSalaryId="userSalaryId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
        <template v-if="value">
            <el-tab-pane label="测试一" name="2" lazy></el-tab-pane>
        </template>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import userSalaryUpdate from './userSalaryUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delUserSalary, getUserSalary} from '@/api/cost/userSalary'

  export default {
    name: 'userSalaryDetail',
    components: {
      userSalaryUpdate,
    },
    provide() {
      return {
        handleComplete: this.handleComplete
      }
    },
    inject: ['handleQuery'],
    data() {
      return {
        title: '',
        userSalaryForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        userSalaryId: null,
        edit: false,  //开启编辑
        disabled: false,
      }
    },
    props: {
      show: {
        type: Boolean,
        default: false
      },
      value: {
        type: Number
      }
    },
    watch: {
      show(data) {
        this.cardShow = data
      },
      value(data) {
        this.activeName = '1'
        this.cardKey = data
        if (data) {
            this.disabled = true
        } else {
            this.disabled = false
        }
        this.getDetail()
      }
    },
    created() {
      this.getDetail()
    },
    mounted() {
    },
    methods: {
      /** 修改操作 */
      handleUpdate() {
        this.disabled = false
      },

      /** 关闭 */
      handleClose() {
        this.cardShow = false
        this.cardKey = null
        this.$emit('handleClose')
      },

      // 获取企业人员工资记录;详情
      getDetail() {
        let userSalaryId = this.value
        if (isNullOrEmpty(userSalaryId)) {
          //新增
          this.title = '新增企业人员工资记录;'
          this.userSalaryId = null
          this.tags = null
        } else {
          getUserSalary(userSalaryId).then(res => {
            let data = res.data
            this.userSalaryForm = res.data
            this.title = '企业人员工资记录;详情'
            this.userSalaryId = data.id
            this.tags = [{'创建人': data.createBy}, {'创建时间': data.createTime}, {'更新人': data.updateBy}, {'更新时间': data.updateTime}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.userSalaryId;
        this.$confirm('是否确认撤销企业人员工资记录;编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delUserSalary(ids);
        }).then(() => {
          this.msgSuccess("删除成功");
          this.handleClose()
          this.$emit('handleQuery')
        }).catch(() => {
        })
      },
    }
  }
</script>
