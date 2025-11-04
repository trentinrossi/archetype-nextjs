'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account, UpdateAccountRequest } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function EditAccountPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateAccountRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);

  useEffect(() => {
    if (params.id) {
      fetchAccount(params.id as string);
    }
  }, [params.id]);

  const fetchAccount = async (id: string) => {
    try {
      const data = await accountService.getAccountById(id);
      setFormData({
        activeStatus: data.activeStatus,
        currentBalance: data.currentBalance,
        creditLimit: data.creditLimit,
        cashCreditLimit: data.cashCreditLimit,
        openDate: data.openDate,
        expirationDate: data.expirationDate,
        reissueDate: data.reissueDate,
        currentCycleCredit: data.currentCycleCredit,
        currentCycleDebit: data.currentCycleDebit,
        addressZipCode: data.addressZipCode,
        groupId: data.groupId,
      });
    } catch (err) {
      console.error('Failed to load account:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    try {
      setSaving(true);
      await accountService.updateAccount(params.id as string, formData);
      router.push(`/accounts/${params.id}`);
    } catch (err) {
      alert('Failed to update account');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;

  return (
    <div className="p-6 max-w-4xl">
      <h1 className="text-2xl font-bold mb-6">Edit Account</h1>

      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="grid grid-cols-2 gap-4">
          <Input
            label="Active Status"
            value={formData.activeStatus || ''}
            onChange={(e) => setFormData({ ...formData, activeStatus: e.target.value })}
            maxLength={1}
          />

          <Input
            label="Current Balance"
            type="number"
            step="0.01"
            value={formData.currentBalance || 0}
            onChange={(e) => setFormData({ ...formData, currentBalance: Number(e.target.value) })}
          />

          <Input
            label="Credit Limit"
            type="number"
            step="0.01"
            value={formData.creditLimit || 0}
            onChange={(e) => setFormData({ ...formData, creditLimit: Number(e.target.value) })}
          />

          <Input
            label="Cash Credit Limit"
            type="number"
            step="0.01"
            value={formData.cashCreditLimit || 0}
            onChange={(e) => setFormData({ ...formData, cashCreditLimit: Number(e.target.value) })}
          />

          <Input
            label="Group ID"
            value={formData.groupId || ''}
            onChange={(e) => setFormData({ ...formData, groupId: e.target.value })}
            maxLength={10}
          />

          <Input
            label="Open Date"
            type="date"
            value={formData.openDate || ''}
            onChange={(e) => setFormData({ ...formData, openDate: e.target.value })}
          />

          <Input
            label="Expiration Date"
            type="date"
            value={formData.expirationDate || ''}
            onChange={(e) => setFormData({ ...formData, expirationDate: e.target.value })}
          />

          <Input
            label="Reissue Date"
            type="date"
            value={formData.reissueDate || ''}
            onChange={(e) => setFormData({ ...formData, reissueDate: e.target.value })}
          />

          <Input
            label="Address ZIP Code"
            value={formData.addressZipCode || ''}
            onChange={(e) => setFormData({ ...formData, addressZipCode: e.target.value })}
            maxLength={10}
          />

          <Input
            label="Current Cycle Credit"
            type="number"
            step="0.01"
            value={formData.currentCycleCredit || 0}
            onChange={(e) => setFormData({ ...formData, currentCycleCredit: Number(e.target.value) })}
          />

          <Input
            label="Current Cycle Debit"
            type="number"
            step="0.01"
            value={formData.currentCycleDebit || 0}
            onChange={(e) => setFormData({ ...formData, currentCycleDebit: Number(e.target.value) })}
          />
        </div>

        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/accounts/${params.id}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
