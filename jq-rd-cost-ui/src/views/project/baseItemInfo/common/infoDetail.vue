<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[infoForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['system:info:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['system:info:remove']">
          撤销
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="项目管理" name="1">
            <info-update :infoId="infoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import infoUpdate from './infoUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delBaseItemInfo, getBaseItemInfo} from '@/api/project/baseItemInfo.js'

  export default {
    name: 'infoDetail',
    components: {
      infoUpdate,
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
        infoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        infoId: null,
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
      },
      row:{
        type:Object
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

      // 获取项目管理;详情
      getDetail() {
        let infoId = this.value
        if (isNullOrEmpty(infoId)) {
          //新增
          this.title = '新增项目管理;'
          this.infoId = null
          this.tags = null
        } else {
          getBaseItemInfo(infoId).then(res => {
            let data = res.data
            this.infoForm = res.data
            this.title = '项目管理详情'
            this.infoId = data.id
            console.log("******",this.row)
            this.tags = [{'项目名称': data.itemName}, {'申报年份': data.applyDate==null?null:data.applyDate.substr(0,4)}, {'项目状态': this.row.itemStatus}, {'项目负责人': this.row.sysUserNickName}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.infoId;
        this.$confirm('是否确认撤销项目管理;编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delBaseItemInfo(ids);
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
