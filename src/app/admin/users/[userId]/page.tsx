'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { adminUserService } from '@/services/adminUserService';
import { AdminUser } from '@/types/admin-user';
import { Button } from '@/components/ui';

export default function AdminUserDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [adminUser, setAdminUser] = useState<AdminUser | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.userId) {
      fetchAdminUser(params.userId as string);
    }
  }, [params.userId]);

  const fetchAdminUser = async (userId: string) => {
    try {
      const data = await adminUserService.getAdminUserById(userId);
      setAdminUser(data);
    } catch (err) {
      console.error('Failed to load admin user:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this admin user?')) return;
    
    try {
      await adminUserService.deleteAdminUser(params.userId as string);
      router.push('/admin/users');
    } catch (err) {
      alert('Failed to delete admin user');
      console.error(err);
    }
  };

  const handleAuthenticate = async () => {
    try {
      await adminUserService.authenticateAdminUser(params.userId as string);
      fetchAdminUser(params.userId as string);
    } catch (err) {
      alert('Failed to authenticate admin user');
      console.error(err);
    }
  };

  const handleDeauthenticate = async () => {
    try {
      await adminUserService.deauthenticateAdminUser(params.userId as string);
      fetchAdminUser(params.userId as string);
    } catch (err) {
      alert('Failed to deauthenticate admin user');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!adminUser) return <div className="p-6">Admin user not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Admin User Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/admin/users/${adminUser.userId}/edit`)}>
            Edit
          </Button>
          {adminUser.authenticationStatus ? (
            <Button variant="secondary" onClick={handleDeauthenticate}>
              Deauthenticate
            </Button>
          ) : (
            <Button onClick={handleAuthenticate}>
              Authenticate
            </Button>
          )}
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/admin/users')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 space-y-4">
        <div>
          <label className="block text-sm font-semibold text-gray-700">User ID</label>
          <p className="mt-1 text-gray-900 text-lg">{adminUser.userId}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Authentication Status</label>
          <p className="mt-1">
            <span className={`px-3 py-1 rounded-full text-sm font-semibold ${
              adminUser.authenticationStatus 
                ? 'bg-green-100 text-green-800' 
                : 'bg-gray-100 text-gray-800'
            }`}>
              {adminUser.authenticationStatus ? 'Authenticated' : 'Not Authenticated'}
            </span>
          </p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Created At</label>
          <p className="mt-1 text-gray-900">{new Date(adminUser.createdAt).toLocaleString()}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Updated At</label>
          <p className="mt-1 text-gray-900">{new Date(adminUser.updatedAt).toLocaleString()}</p>
        </div>
      </div>
    </div>
  );
}
