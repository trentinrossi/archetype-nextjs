'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { adminUserService } from '@/services/adminUserService';
import { AdminUser } from '@/types/admin-user';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button } from '@/components/ui';

export default function AdminUsersPage() {
  const router = useRouter();
  const [adminUsers, setAdminUsers] = useState<AdminUser[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchAdminUsers = useCallback(async () => {
    try {
      setLoading(true);
      const response = await adminUserService.getAdminUsers(0, 100);
      setAdminUsers(response.content || response);
      setError(null);
    } catch (err) {
      setError('Failed to load admin users');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchAdminUsers();
  }, [fetchAdminUsers]);

  const handleDelete = async (userId: string) => {
    if (!confirm(`Are you sure you want to delete admin user ${userId}?`)) return;
    
    try {
      await adminUserService.deleteAdminUser(userId);
      fetchAdminUsers();
    } catch (err) {
      alert('Failed to delete admin user');
      console.error(err);
    }
  };

  const handleAuthenticate = async (userId: string) => {
    try {
      await adminUserService.authenticateAdminUser(userId);
      fetchAdminUsers();
    } catch (err) {
      alert('Failed to authenticate admin user');
      console.error(err);
    }
  };

  const handleDeauthenticate = async (userId: string) => {
    try {
      await adminUserService.deauthenticateAdminUser(userId);
      fetchAdminUsers();
    } catch (err) {
      alert('Failed to deauthenticate admin user');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Admin Users</h1>
          <p className="text-gray-600 mt-1">Manage administrative user accounts and authentication</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={() => router.push('/admin/users/new')}>
            Create Admin User
          </Button>
          <Button variant="secondary" onClick={() => router.push('/admin/menu')}>
            Back to Menu
          </Button>
        </div>
      </div>
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>User ID</TableHead>
            <TableHead>Authentication Status</TableHead>
            <TableHead>Created At</TableHead>
            <TableHead>Updated At</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {adminUsers.length === 0 ? (
            <TableRow>
              <TableCell colSpan={5} className="text-center text-gray-500 py-8">
                No admin users found
              </TableCell>
            </TableRow>
          ) : (
            adminUsers.map((user) => (
              <TableRow key={user.userId}>
                <TableCell>
                  <div className="cursor-pointer font-semibold" onClick={() => router.push(`/admin/users/${user.userId}`)}>
                    {user.userId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/admin/users/${user.userId}`)}>
                    <span className={`px-3 py-1 rounded-full text-xs font-semibold ${
                      user.authenticationStatus 
                        ? 'bg-green-100 text-green-800' 
                        : 'bg-gray-100 text-gray-800'
                    }`}>
                      {user.authenticationStatus ? 'Authenticated' : 'Not Authenticated'}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/admin/users/${user.userId}`)}>
                    {new Date(user.createdAt).toLocaleString()}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/admin/users/${user.userId}`)}>
                    {new Date(user.updatedAt).toLocaleString()}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="flex gap-2">
                    <Button 
                      size="sm" 
                      onClick={(e) => {
                        e.stopPropagation();
                        router.push(`/admin/users/${user.userId}/edit`);
                      }}
                    >
                      Edit
                    </Button>
                    {user.authenticationStatus ? (
                      <Button 
                        size="sm" 
                        variant="secondary"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleDeauthenticate(user.userId);
                        }}
                      >
                        Deauth
                      </Button>
                    ) : (
                      <Button 
                        size="sm" 
                        onClick={(e) => {
                          e.stopPropagation();
                          handleAuthenticate(user.userId);
                        }}
                      >
                        Auth
                      </Button>
                    )}
                    <Button 
                      size="sm" 
                      variant="danger" 
                      onClick={(e) => {
                        e.stopPropagation();
                        handleDelete(user.userId);
                      }}
                    >
                      Delete
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>
    </div>
  );
}
