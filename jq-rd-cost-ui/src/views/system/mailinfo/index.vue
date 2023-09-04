<template>
  <div class="app-container">
    <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
      <el-row>
        <el-col :span="6">
          <el-form-item label="发送时间" prop="sendTime">
            <el-date-picker clearable
                            v-model="queryParams.sendTime"
                            class="form-control"
                            type="date"
                            value-format="yyyy-MM-dd"
                            placeholder="选择发送时间">
            </el-date-picker>
          </el-form-item>
        </el-col>
        <el-col :span="6">
          <el-form-item label="收件人邮箱" prop="receiveUserMail">
            <el-input
              v-model="queryParams.receiveUserMail"
              placeholder="请输入收件人邮箱"
              clearable

              class="form-control"
              @keyup.enter.native="handleQuery"
            />
          </el-form-item>
        </el-col>
        <el-col :span="6">
          <el-form-item label="邮件内容" prop="content">
            <el-input
              v-model="queryParams.content"
              placeholder="请输入邮件内容"
              clearable

              class="form-control"
              @keyup.enter.native="handleQuery"
            />
          </el-form-item>
        </el-col>
        <el-col :span="6">
          <el-form-item label="发送状态" prop="status">
            <el-select v-model="queryParams.status" class="form-control" placeholder="请选择发送状态" clearable>
              <el-option
                v-for="dict in statusOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              />
            </el-select>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="6">
          <el-form-item>
            <el-button type="cyan" icon="el-icon-search" @click="handleQuery">搜索</el-button>
            <el-button icon="el-icon-refresh" @click="resetQuery">重置</el-button>
          </el-form-item>
        </el-col>
      </el-row>
    </el-form>

    <el-row :gutter="10" class="mb8">
      <el-col :span="1.5">
        <el-button :disabled="multiple" type="primary" @click="handleResend">重新发送</el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" ref="JqTableRef" :showSearch.sync="showSearch"
              @handleSelectionChange="handleSelectionChange">
      <template slot="operate" slot-scope="{scope}">
        <el-button type="text" icon="el-icon-edit" @click="handleUpdate(scope.row)"
                   v-hasPermi="['system:mailinfo:edit']">
          修改
        </el-button>
        <el-button type="text" icon="el-icon-delete" @click="handleDelete(scope.row)"
                   v-hasPermi="['system:mailinfo:remove']">
          删除
        </el-button>
      </template>
    </jq-table>

    <!-- 添加或修改系统邮件管理对话框 -->
    <el-dialog :title="title" :visible.sync="open" width="800px" append-to-body>
      <el-form ref="form" :model="form" :rules="rules" label-width="100px">
        <el-row>
          <el-col :span="12">
            <el-form-item label="发送人" prop="sendUser">
              <el-input v-model="form.sendUserName" placeholder="请输入发送人" readonly/>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="发送人邮件" prop="sendUserMail">
              <el-input v-model="form.sendUserMail" placeholder="请输入发送人邮件" readonly/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="收件人" prop="receiveUser">
              <el-input v-model="form.receiveUserName" placeholder="请输入收件人" readonly/>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="收件人邮件" prop="receiveUserMail">
              <el-input v-model="form.receiveUserMail" placeholder="请输入收件人邮件" readonly/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>

          <el-col :span="24">
            <el-form-item label="邮件主题" prop="subject">
              <el-input v-model="form.subject" placeholder="请输入邮件主题"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-form-item label="邮件内容" prop="content">
            <!-- <editor v-model="form.content" :min-height="192"/>-->
            <wang-editor v-model="form.content" :update.sync="form.content"></wang-editor>
          </el-form-item>

        </el-row>

      </el-form>
      <div slot="footer" class="dialog-footer">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </el-dialog>

    <!---系统邮件管理导出组件 -->
    <export-dialog :show.sync="exportOpen"
                   :type.sync="queryParams.exportType"
                   @handleExport="handleExport"></export-dialog>

  </div>
</template>

<script>
  import JqTable from '@/components/JqTable'
  import JqTableMixin from '@/mixin/JqTable'
  import {
    addMailinfo,
    btnReSend,
    delMailinfo,
    exportMailinfo,
    getMailinfo,
    updateMailinfo
  } from '@/api/system/mailinfo'
  import WangEditor from '@/components/WangEditor'

  export default {
    name: 'Mailinfo',
    mixins: [JqTableMixin],
    data() {
      return {
        // 遮罩层
        loading: true,
        // 选中数组
        ids: [],
        // 非单个禁用
        single: true,
        // 非多个禁用
        multiple: true,
        // 显示搜索条件
        showSearch: true,
        // 总条数
        total: 0,
        statusOptions: [],
        // 系统邮件管理表格数据
        mailinfoList: [],
        // 表头的数据
        tableHeadList: [],
        // 弹出层标题
        title: '',
        // 是否显示弹出层
        open: false,
        // 是否显示数据
        exportTitle: '导出警告',
        // 是否显示导出弹出层
        exportOpen: false,
        //查询排序参数
        defaultSort: [],
        // 查询参数
        queryParams: {
          pageNum: 1,
          pageSize: 15,
          exportType: 1,
          ids: null,
          orders: null,
          menuKey: null,
          sendUser: null,
          sendUserMail: null,
          receiveUser: null,
          receiveUserMail: null,
          sendTime: null,
          subject: null,
          content: null,
          status: null,
          companyId: null
        },
        // 表单参数
        form: {},
        // 表单校验
        rules: {},
        tableConfig: {
          url: '/system/mailinfo/list',
          method: 'get',
          queryParams: null,
          exportUrl: '/system/mailinfo/export'
        }
      }
    },
    components: {
      JqTable,
      WangEditor
    },
    created() {
      /** 是否默认邮件*/
      this.getDicts('success_or_fail').then(response => {
        this.statusOptions = response.data
      })
    },
    methods: {
      // 取消按钮
      cancel() {
        this.open = false
        this.reset()
      },
      // 表单排序
      sortChange(column) {
        this.queryParams.orders = this.sortByColumn(column)
        this.getList()
      },
      // 表单重置
      reset() {
        this.form = {
          //附件列表
          attachmentList: [],
          id: null,
          sendUser: null,
          sendUserMail: null,
          receiveUser: null,
          receiveUserMail: null,
          sendTime: null,
          subject: null,
          content: null,
          status: '0',
          remark: null,
          companyId: null,
          delFlag: null,
          createBy: null,
          createTime: null,
          updateBy: null,
          updateTime: null
        }
        this.resetForm('form')
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
      // 多选框选中数据
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
      },
      /** 新增按钮操作 */
      handleAdd() {
        this.reset()
        this.open = true
        this.title = '添加系统邮件管理'
      },
      /** 修改按钮操作 */
      handleUpdate(row) {
        this.reset()
        const id = row.id || this.ids
        getMailinfo(id).then(response => {
          this.form = response.data
          this.open = true
          this.title = '修改系统邮件管理'
        })
      },

      //重新发送邮件
      handleResend(row) {
        const ids = row.id || this.ids
        this.$confirm('确定重新发送吗?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          //return btnReSend(ids);
          btnReSend(ids).then(response => {
            this.msgSuccess(response.msg)
            this.getJqTableData()
          })
        })

      },

      // 重新发送
      /*function btnReSend(id) {
    dialog.confirm("确定重新发送吗？", function () {
      $.kcppcajax(ctx + "/auth/systeminfo/sysmailinfo/btnReSend.kcppc",
        {id: id},
        function (msg) {//msg为返回的数据，在这里做数据绑定
          if (msg.success) {
            dialog.success(msg.message,
              function () {
                grid.searchPageNotChange();
              }
            );
          } else {
            dialog.warn(msg.message);
          }
        }
      );
    });
  }*/


      /** 提交按钮 */
      submitForm() {
        this.$refs['form'].validate(valid => {
          if (valid) {
            if (this.form.id != null) {
              updateMailinfo(this.form).then(response => {
                this.msgSuccess('修改成功')
                this.open = false
                this.getJqTableData()
              })
            } else {
              addMailinfo(this.form).then(response => {
                this.msgSuccess('新增成功')
                this.open = false
                this.getJqTableData()
              })
            }
          }
        })
      },
      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids
        this.$confirm('是否确认删除系统邮件管理编号为"' + ids + '"的数据项?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          return delMailinfo(ids)
        }).then(() => {
          this.getJqTableData()
          this.msgSuccess('删除成功')
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
        exportMailinfo(queryParams)
        this.exportOpen = false
      }

    }
  }
</script>
