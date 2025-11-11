'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account, UpdateAccountRequest } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function EditAccountPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateAccountRequest>({
    currentBalance: 0,
  });
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const fetchAccount = useCallback(async () => {
    if (!params.accountId) return;
    
    try {
      const accountId = params.accountId as string;
      const data = await accountService.getAccountById(accountId);
      setFormData({
        currentBalance: data.currentBalance,
      });
    } catch (err) {
      setError('Failed to load account');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, [params.accountId]);

  useEffect(() => {
    fetchAccount();
  }, [fetchAccount]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (formData.currentBalance < 0.01) {
      alert('Current balance must be at least $0.01');
      return;
    }
    
    try {
      setSaving(true);
      await accountService.updateAccount(params.accountId as string, formData);
      router.push(`/accounts/${params.accountId}`);
    } catch (err: any) {
      alert(err.message || 'Failed to update account');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Edit Account</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="bg-gray-100 p-4 rounded">
          <label className="block text-sm font-semibold text-gray-700">Account ID</label>
          <p className="mt-1 text-gray-900">{params.accountId}</p>
        </div>

        <Input
          label="Current Balance"
          type="number"
          step="0.01"
          min="0.01"
          value={formData.currentBalance}
          onChange={(e) => setFormData({ ...formData, currentBalance: Number(e.target.value) })}
          required
        />
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/accounts/${params.accountId}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
