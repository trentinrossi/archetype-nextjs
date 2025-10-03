'use client';

import { useRouter } from 'next/navigation';
import { useParams } from 'next/navigation';
import Link from 'next/link';
import UserUpdate from '@/components/UserUpdate';
import { UpdateUserRequest } from '@/types/user';

interface BreadcrumbItem {
  label: string;
  href?: string;
}

export default function EditUserPage() {
  const router = useRouter();
  const params = useParams();
  const userId = params?.id as string;

  const breadcrumbs: BreadcrumbItem[] = [
    { label: 'Home', href: '/' },
    { label: 'User Management', href: '/users' },
    { label: `Edit User: ${userId}` },
  ];

  const handleUpdate = async (userId: string, userData: UpdateUserRequest) => {
    // UserUpdate component handles the API call
    // After successful update, stay on the form
  };

  const handleUpdateAndExit = async (userId: string, userData: UpdateUserRequest) => {
    // UserUpdate component handles the API call
    // After successful update, redirect to users list
    router.push('/users');
  };

  const handleCancel = () => {
    router.push('/users');
  };

  if (!userId) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-4">Invalid User ID</h1>
          <Link
            href="/users"
            className="text-blue-600 hover:text-blue-800 font-medium"
          >
            Return to User Management
          </Link>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="py-4">
            {/* Breadcrumbs */}
            <nav className="flex mb-4" aria-label="Breadcrumb">
              <ol className="flex items-center space-x-2">
                {breadcrumbs.map((item, index) => (
                  <li key={index} className="flex items-center">
                    {index > 0 && (
                      <span className="mx-2 text-gray-400">/</span>
                    )}
                    {item.href ? (
                      <Link
                        href={item.href}
                        className="text-blue-600 hover:text-blue-800 text-sm font-medium"
                      >
                        {item.label}
                      </Link>
                    ) : (
                      <span className="text-gray-900 text-sm font-medium">
                        {item.label}
                      </span>
                    )}
                  </li>
                ))}
              </ol>
            </nav>

            {/* Page Title */}
            <div>
              <h1 className="text-3xl font-bold text-gray-900">
                Edit User: {userId}
              </h1>
              <p className="text-gray-600 mt-1">
                Update user information and settings
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="bg-white rounded-lg shadow-sm border">
          <UserUpdate
            initialUserId={userId}
            onUpdate={handleUpdate}
            onUpdateAndExit={handleUpdateAndExit}
            onCancel={handleCancel}
          />
        </div>
      </div>
    </div>
  );
}