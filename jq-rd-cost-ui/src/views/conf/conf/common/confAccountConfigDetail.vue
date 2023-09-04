<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[confAccountConfigForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['conf:confAccountConfig:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['conf:confAccountConfig:remove']">
          删除
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="会计科目配置" name="1">
            <confAccountConfig-update :confAccountConfigId="confAccountConfigId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import confAccountConfigUpdate from './confAccountConfigUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delConfAccountConfig, getConfAccountConfig} from '@/api/conf/confAccountConfig'

  export default {
    name: 'confAccountConfigDetail',
    components: {
      confAccountConfigUpdate,
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
        confAccountConfigForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        confAccountConfigId: null,
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

      // 获取会计科目配置详情
      getDetail() {
        let confAccountConfigId = this.value
        if (isNullOrEmpty(confAccountConfigId)) {
          //新增
          this.title = '新增会计科目配置'
          this.confAccountConfigId = null
          this.tags = null
        } else {
          getConfAccountConfig(confAccountConfigId).then(res => {
            let data = res.data
            this.confAccountConfigForm = res.data
            this.title = '会计科目配置详情'
            this.confAccountConfigId = data.id
            this.tags = [{'创建人': data.createBy}, {'创建时间': data.createTime}, {'更新人': data.updateBy}, {'更新时间': data.updateTime}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.confAccountConfigId;
        this.$confirm('是否确认删除会计科目配置编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delConfAccountConfig(ids);
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
