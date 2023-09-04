<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[userInfoForm.dealUserId]">-->
      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                 v-hasPermi="['cost:userInfo:edit']"
      >
        修改
      </el-button>
      <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:userInfo:remove']"
      >
        删除
      </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="企业人员信息" name="1">
          <userInfo-update :userInfoId="value" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
import userInfoUpdate from './userInfoUpdate'
import { isNullOrEmpty } from '@/utils/jq'
import { delUserInfo, getUserInfo } from '@/api/cost/userInfo'

export default {
  name: 'userInfoDetail',
  components: {
    userInfoUpdate
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
      userInfoForm: {},
      activeName: '1',
      cardShow: false,
      cardKey: null,
      tags: [],
      userInfoId: null,
      edit: false,  //开启编辑
      disabled: false
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

    // 获取企业人员信息详情
    getDetail() {
      let userInfoId = this.value
      if (isNullOrEmpty(userInfoId)) {
        //新增
        this.title = '新增人员'
        this.userInfoForm = {}
        this.userInfoId = null
        this.tags = null
      } else {
        getUserInfo(userInfoId).then(res => {
          this.title = '人员详情'
          let data = res.data
          this.userInfoForm = res.data
          this.userInfoId = data.id
          this.tags = [{ '姓名': data.name }, { '工号': data.jobNumber }, { '专业': data.speciality }, { '更新时间': data.updateTime }]
        })
      }
    },

    /** 撤销 */
    handleRevoke() {
      const ids = this.userInfoId
      this.$confirm('是否确认撤销企业人员信息编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delUserInfo(ids)
      }).then(() => {
        this.msgSuccess('删除成功')
        this.handleClose()
        this.handleQuery()
      }).catch(() => {
      })
    }
  }
}
</script>
