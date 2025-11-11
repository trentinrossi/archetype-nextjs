'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account, UpdateAccountRequest } from '@/types/account';
import { Input, Select, Button } from '@/components/ui';

export default function EditAccountPage() {
  const params = useParams();
  const router = useRouter();
  const [originalAccount, setOriginalAccount] = useState<Account | null>(null);
  const [formData, setFormData] = useState<UpdateAccountRequest>({
    acctId: '',
  });
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    if (params.id) {
      fetchAccount(params.id as string);
    }
  }, [params.id]);

  const fetchAccount = async (id: string) => {
    try {
      setLoading(true);
      const data = await accountService.getAccountById(id);
      setOriginalAccount(data);
      setFormData({
        acctId: data.acctId,
        acctActiveStatus: data.acctActiveStatus,
        acctCurrBal: data.acctCurrBal,
        acctCreditLimit: data.acctCreditLimit,
        acctCashCreditLimit: data.acctCashCreditLimit,
        acctOpenDate: data.acctOpenDate,
        acctExpirationDate: data.acctExpirationDate,
        acctReissueDate: data.acctReissueDate || '',
        acctCurrCycCredit: data.acctCurrCycCredit,
        acctCurrCycDebit: data.acctCurrCycDebit,
        acctGroupId: data.acctGroupId || '',
      });
    } catch (err) {
      console.error('Failed to load account:', err);
      alert('Failed to load account');
      router.push('/accounts');
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    // Balance validation
    if (formData.acctCurrBal !== undefined && formData.acctCurrBal < 0) {
      newErrors.acctCurrBal = 'Current balance cannot be negative';
    }

    // Credit limit validation
    if (formData.acctCreditLimit !== undefined && formData.acctCreditLimit < 0) {
      newErrors.acctCreditLimit = 'Credit limit cannot be negative';
    }

    // Cash credit limit validation
    if (
      formData.acctCashCreditLimit !== undefined &&
      formData.acctCashCreditLimit < 0
    ) {
      newErrors.acctCashCreditLimit = 'Cash credit limit cannot be negative';
    }

    // Expiration date validation
    if (
      formData.acctExpirationDate &&
      formData.acctOpenDate &&
      new Date(formData.acctExpirationDate) <= new Date(formData.acctOpenDate)
    ) {
      newErrors.acctExpirationDate = 'Expiration date must be after open date';
    }

    // Reissue date validation (if provided)
    if (
      formData.acctReissueDate &&
      formData.acctOpenDate &&
      new Date(formData.acctReissueDate) <= new Date(formData.acctOpenDate)
    ) {
      newErrors.acctReissueDate = 'Reissue date must be after open date';
    }

    // Current cycle credit validation
    if (
      formData.acctCurrCycCredit !== undefined &&
      formData.acctCurrCycCredit < 0
    ) {
      newErrors.acctCurrCycCredit = 'Current cycle credit cannot be negative';
    }

    // Current cycle debit validation
    if (formData.acctCurrCycDebit !== undefined && formData.acctCurrCycDebit < 0) {
      newErrors.acctCurrCycDebit = 'Current cycle debit cannot be negative';
    }

    // Group ID validation (max 10 characters)
    if (formData.acctGroupId && formData.acctGroupId.length > 10) {
      newErrors.acctGroupId = 'Group ID cannot exceed 10 characters';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      return;
    }

    try {
      setSaving(true);

      // Prepare data - remove empty optional fields
      const submitData: UpdateAccountRequest = {
        ...formData,
      };

      if (!submitData.acctReissueDate) {
        delete submitData.acctReissueDate;
      }

      if (!submitData.acctGroupId) {
        delete submitData.acctGroupId;
      }

      await accountService.updateAccount(params.id as string, submitData);
      router.push(`/accounts/${params.id}`);
    } catch (err: any) {
      alert(err.message || 'Failed to update account');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    router.push(`/accounts/${params.id}`);
  };

  if (loading) return <div className="p-6">Loading account...</div>;
  if (!originalAccount) return <div className="p-6">Account not found</div>;

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Edit Account</h1>

      <form onSubmit={handleSubmit} className="space-y-6">
        {/* Account Identification */}
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Account Identification
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <Input
                label="Account ID"
                value={formData.acctId}
                disabled
                readOnly
              />
              <p className="mt-1 text-xs text-gray-500">
                Account ID cannot be changed
              </p>
            </div>
            <div>
              <Select
                label="Active Status *"
                value={formData.acctActiveStatus || 'A'}
                onChange={(e) =>
                  setFormData({
                    ...formData,
                    acctActiveStatus: e.target.value as 'A' | 'I',
                  })
                }
                options={[
                  { value: 'A', label: 'Active' },
                  { value: 'I', label: 'Inactive' },
                ]}
                required
              />
            </div>
            <div className="col-span-2">
              <Input
                label="Group ID"
                value={formData.acctGroupId || ''}
                onChange={(e) =>
                  setFormData({ ...formData, acctGroupId: e.target.value })
                }
                placeholder="Optional group identifier"
                maxLength={10}
                error={errors.acctGroupId}
              />
              <p className="mt-1 text-xs text-gray-500">
                Optional, maximum 10 characters
              </p>
            </div>
          </div>
        </div>

        {/* Financial Information */}
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Financial Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <Input
                label="Current Balance *"
                type="number"
                step="0.01"
                value={formData.acctCurrBal || 0}
                onChange={(e) =>
                  setFormData({
                    ...formData,
                    acctCurrBal: parseFloat(e.target.value) || 0,
                  })
                }
                required
                error={errors.acctCurrBal}
              />
            </div>
            <div>
              <Input
                label="Credit Limit *"
                type="number"
                step="0.01"
                value={formData.acctCreditLimit || 0}
                onChange={(e) =>
                  setFormData({
                    ...formData,
                    acctCreditLimit: parseFloat(e.target.value) || 0,
                  })
                }
                required
                error={errors.acctCreditLimit}
              />
            </div>
            <div>
              <Input
                label="Cash Credit Limit *"
                type="number"
                step="0.01"
                value={formData.acctCashCreditLimit || 0}
                onChange={(e) =>
                  setFormData({
                    ...formData,
                    acctCashCreditLimit: parseFloat(e.target.value) || 0,
                  })
                }
                required
                error={errors.acctCashCreditLimit}
              />
            </div>
          </div>
        </div>

        {/* Current Cycle Information */}
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Current Cycle Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <Input
                label="Current Cycle Credit *"
                type="number"
                step="0.01"
                value={formData.acctCurrCycCredit || 0}
                onChange={(e) =>
                  setFormData({
                    ...formData,
                    acctCurrCycCredit: parseFloat(e.target.value) || 0,
                  })
                }
                required
                error={errors.acctCurrCycCredit}
              />
            </div>
            <div>
              <Input
                label="Current Cycle Debit *"
                type="number"
                step="0.01"
                value={formData.acctCurrCycDebit || 0}
                onChange={(e) =>
                  setFormData({
                    ...formData,
                    acctCurrCycDebit: parseFloat(e.target.value) || 0,
                  })
                }
                required
                error={errors.acctCurrCycDebit}
              />
            </div>
          </div>
        </div>

        {/* Date Information */}
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Date Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <Input
                label="Open Date *"
                type="date"
                value={formData.acctOpenDate || ''}
                onChange={(e) =>
                  setFormData({ ...formData, acctOpenDate: e.target.value })
                }
                required
                error={errors.acctOpenDate}
              />
            </div>
            <div>
              <Input
                label="Expiration Date *"
                type="date"
                value={formData.acctExpirationDate || ''}
                onChange={(e) =>
                  setFormData({ ...formData, acctExpirationDate: e.target.value })
                }
                required
                error={errors.acctExpirationDate}
              />
            </div>
            <div>
              <Input
                label="Reissue Date"
                type="date"
                value={formData.acctReissueDate || ''}
                onChange={(e) =>
                  setFormData({ ...formData, acctReissueDate: e.target.value })
                }
                error={errors.acctReissueDate}
              />
              <p className="mt-1 text-xs text-gray-500">
                Optional, must be after open date
              </p>
            </div>
          </div>
        </div>

        {/* Computed Fields Display (Read-only) */}
        <div className="bg-gray-50 shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Computed Information (Read-only)
          </h2>
          <div className="grid grid-cols-2 gap-4 text-sm">
            <div>
              <label className="block font-semibold text-gray-700">
                Available Credit
              </label>
              <p className="mt-1 text-gray-900">
                {new Intl.NumberFormat('en-US', {
                  style: 'currency',
                  currency: 'USD',
                }).format(originalAccount.availableCredit)}
              </p>
            </div>
            <div>
              <label className="block font-semibold text-gray-700">
                Available Cash Credit
              </label>
              <p className="mt-1 text-gray-900">
                {new Intl.NumberFormat('en-US', {
                  style: 'currency',
                  currency: 'USD',
                }).format(originalAccount.availableCashCredit)}
              </p>
            </div>
            <div>
              <label className="block font-semibold text-gray-700">
                Current Cycle Net Amount
              </label>
              <p className="mt-1 text-gray-900">
                {new Intl.NumberFormat('en-US', {
                  style: 'currency',
                  currency: 'USD',
                }).format(originalAccount.currentCycleNetAmount)}
              </p>
            </div>
            <div>
              <label className="block font-semibold text-gray-700">
                Account Status
              </label>
              <p className="mt-1 text-gray-900">
                {originalAccount.isActive ? 'Active' : 'Inactive'} |{' '}
                {originalAccount.isExpired ? 'Expired' : 'Valid'}
              </p>
            </div>
          </div>
          <p className="mt-4 text-xs text-gray-500">
            These values are automatically calculated based on the account data
          </p>
        </div>

        {/* Form Actions */}
        <div className="flex gap-4 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving Changes...' : 'Save Changes'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
