'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { CreateAccountRequest } from '@/types/account';
import { Input, Select, Button } from '@/components/ui';

export default function CreateAccountPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateAccountRequest>({
    acctId: '',
    acctActiveStatus: 'A',
    acctCurrBal: 0,
    acctCreditLimit: 0,
    acctCashCreditLimit: 0,
    acctOpenDate: new Date().toISOString().split('T')[0],
    acctExpirationDate: '',
    acctReissueDate: '',
    acctCurrCycCredit: 0,
    acctCurrCycDebit: 0,
    acctGroupId: '',
  });
  const [loading, setLoading] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    // Account ID validation - must be exactly 11 digits
    if (!formData.acctId) {
      newErrors.acctId = 'Account ID is required';
    } else if (!/^\d{11}$/.test(formData.acctId)) {
      newErrors.acctId = 'Account ID must be exactly 11 numeric digits';
    }

    // Balance validation
    if (formData.acctCurrBal < 0) {
      newErrors.acctCurrBal = 'Current balance cannot be negative';
    }

    // Credit limit validation
    if (formData.acctCreditLimit < 0) {
      newErrors.acctCreditLimit = 'Credit limit cannot be negative';
    }

    // Cash credit limit validation
    if (formData.acctCashCreditLimit < 0) {
      newErrors.acctCashCreditLimit = 'Cash credit limit cannot be negative';
    }

    // Open date validation
    if (!formData.acctOpenDate) {
      newErrors.acctOpenDate = 'Open date is required';
    }

    // Expiration date validation
    if (!formData.acctExpirationDate) {
      newErrors.acctExpirationDate = 'Expiration date is required';
    } else if (
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
    if (formData.acctCurrCycCredit < 0) {
      newErrors.acctCurrCycCredit = 'Current cycle credit cannot be negative';
    }

    // Current cycle debit validation
    if (formData.acctCurrCycDebit < 0) {
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
      setLoading(true);
      
      // Prepare data - remove empty optional fields
      const submitData: CreateAccountRequest = {
        ...formData,
      };
      
      if (!submitData.acctReissueDate) {
        delete submitData.acctReissueDate;
      }
      
      if (!submitData.acctGroupId) {
        delete submitData.acctGroupId;
      }

      await accountService.createAccount(submitData);
      router.push('/accounts');
    } catch (err: any) {
      alert(err.message || 'Failed to create account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleCancel = () => {
    router.push('/accounts');
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Create New Account</h1>

      <form onSubmit={handleSubmit} className="space-y-6">
        {/* Account Identification */}
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Account Identification
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <Input
                label="Account ID *"
                value={formData.acctId}
                onChange={(e) =>
                  setFormData({ ...formData, acctId: e.target.value })
                }
                placeholder="11-digit numeric ID"
                required
                error={errors.acctId}
                maxLength={11}
              />
              <p className="mt-1 text-xs text-gray-500">
                Must be exactly 11 numeric digits
              </p>
            </div>
            <div>
              <Select
                label="Active Status *"
                value={formData.acctActiveStatus}
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
                value={formData.acctGroupId}
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
                value={formData.acctCurrBal}
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
                value={formData.acctCreditLimit}
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
                value={formData.acctCashCreditLimit}
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
                value={formData.acctCurrCycCredit}
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
                value={formData.acctCurrCycDebit}
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
                value={formData.acctOpenDate}
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
                value={formData.acctExpirationDate}
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
                value={formData.acctReissueDate}
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

        {/* Form Actions */}
        <div className="flex gap-4 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating Account...' : 'Create Account'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
