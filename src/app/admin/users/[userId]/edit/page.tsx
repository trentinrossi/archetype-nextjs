'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { adminUserService } from '@/services/adminUserService';
import { UpdateAdminUserRequest } from '@/types/admin-user';
import { Button } from '@/components/ui';

export default function EditAdminUserPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateAdminUserRequest>({
    authenticationStatus: false,
  });
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);

  useEffect(() => {
    if (params.userId) {
      fetchAdminUser(params.userId as string);
    }
  }, [params.userId]);

  const fetchAdminUser = async (userId: string) => {
    try {
      const data = await adminUserService.getAdminUserById(userId);
      setFormData({
        authenticationStatus: data.authenticationStatus,
      });
    } catch (err) {
      console.error('Failed to load admin user:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    try {
      setSaving(true);
      await adminUserService.updateAdminUser(params.userId as string, formData);
      router.push(`/admin/users/${params.userId}`);
    } catch (err) {
      alert('Failed to update admin user');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Edit Admin User</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div className="bg-gray-50 p-4 rounded">
          <label className="block text-sm font-semibold text-gray-700">User ID</label>
          <p className="mt-1 text-gray-900 text-lg">{params.userId}</p>
          <p className="mt-1 text-sm text-gray-500">User ID cannot be changed</p>
        </div>
        
        <div className="flex items-center">
          <input
            type="checkbox"
            id="authenticationStatus"
            checked={formData.authenticationStatus}
            onChange={(e) => setFormData({ ...formData, authenticationStatus: e.target.checked })}
            className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
          />
          <label htmlFor="authenticationStatus" className="ml-2 block text-sm text-gray-900">
            Authenticated
          </label>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/admin/users/${params.userId}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
