<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[deviceInfoForm.dealUserId]">-->
      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                 v-hasPermi="['cost:deviceInfo:edit']"
      >
        修改
      </el-button>
      <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:deviceInfo:remove']"
      >
        删除
      </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="企业设备信息" name="1">
          <deviceInfo-update :deviceInfoId="deviceInfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
import deviceInfoUpdate from './deviceInfoUpdate'
import { isNullOrEmpty } from '@/utils/jq'
import { delDeviceInfo, getDeviceInfo } from '@/api/cost/deviceInfo'

export default {
  name: 'deviceInfoDetail',
  components: {
    deviceInfoUpdate
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
      deviceInfoForm: {},
      activeName: '1',
      cardShow: false,
      cardKey: null,
      tags: [],
      deviceInfoId: null,
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

    // 获取企业设备信息详情
    getDetail() {
      let deviceInfoId = this.value
      if (isNullOrEmpty(deviceInfoId)) {
        //新增
        this.title = '新增设备'
        this.deviceInfoForm = {}
        this.disabled = false
        this.deviceInfoId = null
        this.tags = null
      } else {
        getDeviceInfo(deviceInfoId).then(res => {
          this.title = '设备详情'
          let data = res.data
          this.deviceInfoForm = res.data
          this.disabled = true
          this.deviceInfoId = data.id
          this.tags = [{ '设备编码': data.deviceNo }, { '设备名称': data.deviceName }, { '规格型号': data.deviceSpecs }, { '取得日期': data.getDate }]
        })
      }
    },

    /** 撤销 */
    handleRevoke() {
      const ids = this.deviceInfoId
      this.$confirm('是否确认撤销企业设备信息编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delDeviceInfo(ids)
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
